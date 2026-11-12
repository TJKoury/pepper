!$Procedure F_ZZGFSOLVX ( ZZGFSOLV family tests )

      SUBROUTINE F_ZZGFSOLVX ( OK )
      use pepper_legacy

! $ Abstract
!
!     This routine tests the SPICELIB routines
!
!        ZZGFSOLVX
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
!     This family tests the GF event finder ZZGFSOLVX. Code
!     based on the original F_ZZGFSOLVX routine. No changes to test
!     logic.
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
!-    TSPICE Version 1.2.0, 14-OCT-2021 (BVS)
!
!        Bug fix: fixed routine name in CHKIN/CHKOUT calls
!        in entry T_SOLVX (T_ZZGFSOLVX -> T_SOLVX).
!
!-    TSPICE Version 1.1.0, 11-FEB-2017 (EDW) (NJB)
!
!        Eliminated small TOLRNC test cases to correspond to change
!        in ZZGFSOLVX.
!
!-    TSPICE Version 1.0.0, 27-APR-2012 (EDW)
!
!        Code based on F_ZZGFSOLV.
!-&

! $ Index_Entries
!
!   None.
!
!-&

!
!     SPICELIB functions
!


!
!     External routines
!
      EXTERNAL T_UDQLTX
      EXTERNAL T_REPUX
      EXTERNAL T_REFNX
      EXTERNAL UDFUNCX

!
!     Local parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 100 )

!
!     Local Variables
!
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      START
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      TBEG
      DOUBLE PRECISION      TEND
      DOUBLE PRECISION      TOLRNC
      DOUBLE PRECISION      CRIT
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )

      LOGICAL               BAIL
      LOGICAL               CSTEP
      LOGICAL               RPT

      INTEGER               N
      INTEGER               NRPT

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFSOLVX' )

!
!     Set the progress report test hit counter to zero.
!
      CALL ZZRPTIX()


!
!     Case 1
!
      CALL TCASE ( 'TOLRNC = 0' )

      BAIL  = .FALSE.
      CSTEP = .TRUE.
      RPT   = .FALSE.
      STEP  = 1.D0
      START = 0.D0
      FINISH= 10.D0
      TOLRNC= 0.D0

      CALL ZZGFSOLVX( UDFUNCX, T_UDQLTX, GFSTEP,  GFREFN,  BAIL, &
                      GFBAIL,   CSTEP,   STEP,    START, &
                      FINISH,  TOLRNC,  RPT,     GFREPU, &
                      RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )


!
!     Case 2
!
      CALL TCASE ( 'START > FINISH' )

      BAIL  = .FALSE.
      CSTEP = .TRUE.
      RPT   = .FALSE.
      STEP  = 1.D0
      START = 10.D0
      FINISH= 0.D0
      TOLRNC= 1.D0

      CALL ZZGFSOLVX( UDFUNCX, T_UDQLTX, GFSTEP,  GFREFN,  BAIL, &
                      GFBAIL,   CSTEP,   STEP,    START, &
                      FINISH,  TOLRNC,  RPT,     GFREPU, &
                      RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTIMECASE)', OK )



!
!     Case 3
!
      CALL TCASE ( 'Step function, one interval, one critical value.' )

!
!     Set a search to key on the critical value defined in
!     T_ZZGFSOLVX. With a STEP size of one, and a critical value
!     a double precision representation of an integer, the
!     RESULT window returns [CRIT + TOLRNC/2, FINISH].
!
!                            ------------
!                            |
!           -----------------|
!

      CALL SSIZED ( MAXWIN, RESULT )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BAIL  = .FALSE.
      CSTEP = .TRUE.
      RPT   = .TRUE.
      STEP  = 1.D0
      START = 0.D0
      FINISH= 100.D0

!
!     Set a value strictly between START and FINISH.
!
      CALL ZZCRITSX( 93.D0)

!
!     Using bisection for refinement; set TOLRNC as a power of 2.
!
!     TOLRNC = 2**-N (N=3)
!
      TOLRNC= 0.125D0

      CALL ZZGFSOLVX( UDFUNCX, T_UDQLTX, GFSTEP,  GFREFN,  BAIL, &
                      GFBAIL,  CSTEP,   STEP,    START, &
                      FINISH,  TOLRNC,  RPT,     T_REPUX, &
                      RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect one root.
!
      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL WNFETD ( RESULT, N, TBEG, TEND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We know the exact values for the expected interval endpoints.
!
      CALL ZZCRITGX( CRIT )

      CALL CHCKSD ( 'TBEG', TBEG, '=', CRIT+ TOLRNC/2.D0, 0.D0, OK )
      CALL CHCKSD ( 'TBEG', TEND, '=', FINISH,            0.D0, OK )

!
!     We know the exact value for the number of report hits.
!
      CALL ZZNRPTGX( NRPT )

      CALL CHCKSI ( 'NRPT', NRPT, '=', 101, 0, OK)


!
!     Case 4
!
      CALL TCASE ( 'Step function, one interval, two critical values.' )

!
!                            -----------------
!                            |               |
!           -----------------|               |--
!                           CRIT           2*CRIT
!
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BAIL  = .FALSE.
      CSTEP = .TRUE.
      RPT   = .TRUE.
      STEP  = 1.D0
      START = 0.D0
      FINISH= 200.D0

!
!     Set a value strictly between START and FINISH with
!     2*CRIT + TOLRNC/2.D0 < FINISH.
!
      CALL ZZCRITSX ( 93.D0)

!
!     Using bisection for refinement; set TOLRNC as a power of 2.
!
!     TOLRNC = 2**-N (N=3)
!
      TOLRNC= 0.125D0

      CALL ZZGFSOLVX( UDFUNCX, T_UDQLTX, GFSTEP,  GFREFN,  BAIL, &
                      GFBAIL,  CSTEP,   STEP,    START, &
                      FINISH,  TOLRNC,  RPT,     T_REPUX, &
                      RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect one root.
!
      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL WNFETD ( RESULT, N, TBEG, TEND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We know the exact values for the expected interval endpoints.
!
      CALL ZZCRITGX ( CRIT )

      CALL CHCKSD ( 'TBEG', TBEG, '=',      CRIT+ TOLRNC/2.D0, 0.D0, OK)
      CALL CHCKSD ( 'TBEG', TEND, '=', 2.D0*CRIT+ TOLRNC/2.D0, 0.D0, OK)

!
!     We know the exact value for the number of report hits.
!
      CALL ZZNRPTGX ( NRPT )
      CALL CHCKSI ( 'NRPT', NRPT, '=', 200, 0, OK)



!
!     Case 5
!
      CALL TCASE ( 'Check for loop convergence error signal.' )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL SCARDD ( 0,      RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BAIL  = .FALSE.
      CSTEP = .TRUE.
      RPT   = .TRUE.
      STEP  = 1.D0
      START = 0.D0
      FINISH= 100.D0

!
!     Set a value strictly between START and FINISH.
!
      CALL ZZCRITSX( 93.D0)

!
!     Set TOLRNC to something we know will work for bisection.
!
      TOLRNC= 0.125D0

!
!     Use a test version of GFREFN to ensure a slower than
!     bisection convergence rate.
!
      CALL ZZGFSOLVX( UDFUNCX, T_UDQLTX, GFSTEP,  T_REFNX,  BAIL, &
                      GFBAIL,  CSTEP,   STEP,    START, &
                      FINISH,  TOLRNC,  RPT,     T_REPUX, &
                      RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOCONVERG)', OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END



!$Procedure T_SOLVX ( Test utilities )

       SUBROUTINE T_SOLVX ( UDFUNCX, ET,   IN_CON, VAL, &
                            IVBEG,  IVEND, NVAL )
      use pepper_legacy

! $ Abstract
!
!     ZZGFSOLVX test family specific utility code.
!
! $ Required_Reading
!
!   None.
!
! $ Keywords
!
!   None.
!
! $ Declarations

      IMPLICIT NONE

      EXTERNAL UDFUNCX

      DOUBLE PRECISION      ET
      LOGICAL               IN_CON
      DOUBLE PRECISION      VAL
      DOUBLE PRECISION      IVBEG
      DOUBLE PRECISION      IVEND
      INTEGER               NVAL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!
! $ Detailed_Input
!
!   None.
!
! $ Detailed_Output
!
!   None.
!
! $ Parameters
!
!   None.
!
! $ Exceptions
!
!   None.
!
! $ Files
!
!   None.
!
! $ Particulars
!
!   None.
!
! $ Examples
!
!   None.
!
! $ Restrictions
!
!   None.
!
! $ Literature_References
!
!   None.
!
! $ Author_and_Institution
!
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 27-APR-2012 (EDW)
!
!-&

! $ Index_Entries
!
!   None.
!
!-&

      DOUBLE PRECISION      CRIT
      DOUBLE PRECISION      X
      INTEGER               NRPT

      SAVE                  CRIT
      SAVE                  NRPT

      DATA                  NRPT        /0/

      CALL CHKIN  ( 'T_SOLVX'           )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_SOLVX'           )

      RETURN


!
!     A UDCOND test function. The function returns FALSE
!     for ET <= CRIT and ET > 2*CRIT.
!
!
      ENTRY T_UDQLTX ( UDFUNCX, ET, IN_CON )

         IN_CON = .FALSE.

         CALL UDFUNCX ( ET, X )

!
!        The function returns true after the CRIT value.
!
         IF ( X .GT. CRIT ) THEN
            IN_CON = .TRUE.
         END IF

!
!        The function returns false after the 2*CRIT value.
!
         IF ( X .GT. 2.D0*CRIT ) THEN
           IN_CON = .FALSE.
         END IF

      RETURN


!
!     Set the critical value used in the tests.
!
      ENTRY ZZCRITSX( VAL )

         CRIT = VAL

      RETURN


!
!     Return the critical value used in the tests.
!
      ENTRY ZZCRITGX( VAL )

         VAL = CRIT

      RETURN

!
!     A UDREPU test function.
!
      ENTRY T_REPUX( IVBEG, IVEND, ET )

!
!        Count the number of progress report calls.
!
         NRPT = NRPT + 1

      RETURN


!
!     Return the number of progress report calls then reset the
!     count variable to zero.
!
      ENTRY ZZNRPTGX( NVAL )

         NVAL = NRPT
         NRPT = 0

      RETURN


      ENTRY ZZRPTIX( )

         NRPT = 0

      RETURN

      END


!
!     Test version of GFREFN. The EPSILON value causes
!     an increase in the number of refinement steps
!     required to converge as compared to bisection.
!
!     The default GFREFN uses an EPSILON value 0.5.
!
      SUBROUTINE T_REFNX ( T1, T2, S1, S2, T )
      use pepper_legacy
      IMPLICIT NONE

      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      LOGICAL               S1
      LOGICAL               S2
      DOUBLE PRECISION      T

      DOUBLE PRECISION      X
      DOUBLE PRECISION      EPSILON

      EPSILON = 0.999D0

      X = T1 * ( 1.D0 - EPSILON ) + ( T2 * EPSILON )

      T = BRCKTD ( X, T1, T2 )

      RETURN
      END


!$Procedure UDFUNCX ( Test routine )

      SUBROUTINE UDFUNCX ( ET, X )
      use pepper_legacy

! $ Abstract
!
!     GF test function; echo input as output.
!
! $ Required_Reading
!
!   None.
!
! $ Keywords
!
!   None.
!
! $ Declarations

      IMPLICIT              NONE

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      X

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!
! $ Detailed_Input
!
!   None.
!
! $ Detailed_Output
!
!   None.
!
! $ Parameters
!
!   None.
!
! $ Exceptions
!
!   None.
!
! $ Files
!
!   None.
!
! $ Particulars
!
!   None.
!
! $ Examples
!
!   None.
!
! $ Restrictions
!
!   None.
!
! $ Literature_References
!
!   None.
!
! $ Author_and_Institution
!
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 27-APR-2012 (EDW)
!
!-&

! $ Index_Entries
!
!   None.
!
!-&

      X = ET

      RETURN
      END

