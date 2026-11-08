!$Procedure F_ZZDM ( Test ZZDIV, ZZMULT )

      SUBROUTINE F_ZZDM ( OK )

! $ Abstract
!
!     Test family to verify that ZZDIV behaves as expected.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 17-APR-2014 (EDW)
!
!-&

!
!     SPICELIB Functions
!

      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      ZZDIV
      DOUBLE PRECISION      ZZMULT
      DOUBLE PRECISION      T_RANDD


!
!     Local Variables
!

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

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_ZZDM' )

!
!     Initialize the random number seed.
!
      SEED = -6273618

!
!     Initialize the exponent value.
!
      EXPNT = DBLE( INT( LOG10( DPMAX() ) ) )  - 1.D0


!
!     Case 1
!
      CALL TCASE ( 'Safe division check.' )

!
!     Standard, safe evaluation.
!
      NUMR  = 1.D0
      DENOM = 10.D0

      DIV = ZZDIV( NUMR, DENOM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZDIV 1/10', DIV, '~', 0.1D0, 1.D-12, OK )



!
!     Case 2
!
      CALL TCASE ( 'Underflow division check.' )

!
!     A numeric underflow event as defined in ZZDIV. No error,
!     return zero.
!
      NUMR  = 1.D-200
      DENOM = 1.D200

      DIV = ZZDIV( NUMR, DENOM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZDIV 1D-400', DIV, '=', 0.D0, 0.D0, OK )



!
!     Case 3
!
      CALL TCASE ( 'Overflow division check.' )

!
!     A numeric overflow event as defined in ZZDIV.
!
      NUMR  = DPMAX()
      DENOM = 1.D0

      DIV = ZZDIV( NUMR, DENOM )
      CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )

      CALL CHCKSD ( 'ZZDIV DPMAX/1', DIV, '=', 0.D0, 0.D0, OK )


!
!     Case 4
!
      CALL TCASE ( '1/0 division check.' )

!
!     A divide by zero event.
!
      NUMR  = 1.D0
      DENOM = 0.D0

      DIV = ZZDIV( NUMR, DENOM )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)', OK )

      CALL CHCKSD ( 'ZZDIV 1/0', DIV, '=', 0.D0, 0.D0, OK )


!
!     Case 5
!
      CALL TCASE ( '0/0 division check.' )

!
!     A 0/0 event. ZZDIV treats this as a divide by zero
!     event.
!
      NUMR  = 0.D0
      DENOM = 0.D0

      DIV = ZZDIV( NUMR, DENOM )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)', OK )

      CALL CHCKSD ( 'ZZDIV 0/0', DIV, '=', 0.D0, 0.D0, OK )



!
!     Case 6
!
      CALL TCASE ( 'Safe multiplication check.' )

!
!     Standard, safe evaluation.
!
      A = 2.D0
      B = 10.D0

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZMULT 2*10', MULT, '~', 20.D0, 1.D-12, OK )



!
!     Case 7
!
      CALL TCASE ( 'A numeric overflow event (1).' )

      A = 1.D0
      B = DPMAX()

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )

      CALL CHCKSD ( 'ZZMULT overflow (1)', MULT, '=', 0.D0, 1.D-12, OK )



!
!     Case 8
!
      CALL TCASE ( 'A numeric overflow event (2).' )

      A = 1.D208
      B = 1.D307

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )

      CALL CHCKSD ( 'ZZMULT overflow (2)', MULT, '=', 0.D0, 1.D-12, OK )



!
!     Case 9
!
      CALL TCASE ( 'Zero multiplication check (1).' )

      A = 0.D0
      B = 10.D0

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZMULT zero (1)', MULT, '=', 0.D0, 1.D-12, OK )



!
!     Case 10
!
      CALL TCASE ( 'Zero multiplication check (2).' )

      A = 10.D0
      B = 0.D0

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZMULT zero (2)', MULT, '=', 0.D0, 1.D-12, OK )



!
!     Case 11
!
      CALL TCASE ( 'Zero multiplication check (3).' )

      A = 0.D0
      B = 0.D0

      MULT = ZZMULT( A, B )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ZZMULT zero (3)', MULT, '=', 0.D0, 1.D-12, OK )

!
!     Note, the use of 5000 iterations is an arbitrary choice.
!

!
!     Case 12
!
      CALL TCASE ( 'Random multiplication check.' )

      DO I=1, 5000

         A  =  10.D0 ** T_RANDD( -EXPNT, EXPNT, SEED )
         B  =  10.D0 ** T_RANDD( -EXPNT, EXPNT, SEED )

         CONT   = .FALSE.
         LOGVAL = LOG10( DABS(A) ) + LOG10( DABS(B) )

!
!        Pass over the calculation if the log10 of the calculated is not
!        within (-EXPNT, EXPNT).
!
         CONT   = (LOGVAL .LT. EXPNT) .AND. ( LOGVAL .GT. -EXPNT )

         IF ( CONT ) THEN

            MULT  = ZZMULT( A, B )
            CALL CHCKXC ( .FALSE., ' ', OK )

            MULTX = A*B

            CALL CHCKSD ( 'Random multiplication', &
                           MULT, '=', MULTX, 1.D-12, OK )

         END IF

      END DO


!
!     Case 13
!
      CALL TCASE ( 'Random division check.' )

      DO I=1, 5000

         NUMR  =  10.D0 ** T_RANDD( -EXPNT, EXPNT, SEED )
         DENOM =  10.D0 ** T_RANDD( -EXPNT, EXPNT, SEED )

         CONT   = .FALSE.
         LOGVAL = LOG10( DABS(NUMR) ) - LOG10( DABS(DENOM) )

!
!        Pass over the calculation if the log10 of the calculated is not
!        within (-EXPNT, EXPNT) or if the denominator is zero.
!
         CONT   = (LOGVAL .LT. EXPNT) &
                  .AND. &
                  ( LOGVAL .GT. -EXPNT ) &
                  .AND. &
                  ( DENOM .NE. 0.D0 )

         IF ( CONT ) THEN

            DIV  = ZZDIV( NUMR, DENOM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            MULTX = NUMR/DENOM

            CALL CHCKSD ( 'Random division', &
                          DIV, '=', MULTX, 1.D-12, OK )

         END IF

      END DO


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
