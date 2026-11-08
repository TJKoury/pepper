!$Procedure F_ZZGFSOLV ( ZZGFSOLV family tests )

      SUBROUTINE F_ZZGFSOLV ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routines
!
!        ZZGFSOLV
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
!     N.J. Bachman    (JPL)
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 11-FEB-2017 (NJB)(EDW)
!
!        Eliminated small TOLRNC test cases to correspond to change
!        in ZZGFSOLVX.
!
!-    TSPICE Version 1.1.0, 26-APR-2012 (EDW)
!
!        Edited the invalid tolerance error test return
!        short message from SPICE(VALUEOUTOFRANGE) to
!        SPICE(INVALIDTOLERANCE).
!
!-    TSPICE Version 1.0.0, 11-MAR-2009 (EDW)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               WNCARD

      LOGICAL               GFBAIL

!
!     External routines
!
      EXTERNAL              GFBAIL
      EXTERNAL              GFREFN
      EXTERNAL              GFREPU
      EXTERNAL              GFSTEP
      EXTERNAL              T_UDQLT
      EXTERNAL              T_REPU
      EXTERNAL              T_REFN

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
      CALL TOPEN ( 'F_ZZGFSOLV' )

!
!     Set the progress report test hit counter to zero.
!
      CALL ZZRPTI()


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

      CALL ZZGFSOLV ( T_UDQLT, GFSTEP,  GFREFN,  BAIL, &
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

      CALL ZZGFSOLV ( T_UDQLT, GFSTEP,  GFREFN,  BAIL, &
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
!     T_ZZGFSOLV. With a STEP size of one, and a critical value
!     a double precisiion representation of an integer, the
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
      CALL ZZCRITS( 93.D0)

!
!     Using bisection for refinement; set TOLRNC as a power of 2.
!
!     TOLRNC = 2**-N (N=3)
!
      TOLRNC= 0.125D0

      CALL ZZGFSOLV ( T_UDQLT, GFSTEP,  GFREFN,  BAIL, &
                      GFBAIL,  CSTEP,   STEP,    START, &
                      FINISH,  TOLRNC,  RPT,     T_REPU, &
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
      CALL ZZCRITG( CRIT )

      CALL CHCKSD ( 'TBEG', TBEG, '=', CRIT+ TOLRNC/2.D0, 0.D0, OK )
      CALL CHCKSD ( 'TBEG', TEND, '=', FINISH,            0.D0, OK )

!
!     We know the exact value for the number of report hits.
!
      CALL ZZNRPTG( NRPT )

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
      CALL ZZCRITS ( 93.D0)

!
!     Using bisection for refinement; set TOLRNC as a power of 2.
!
!     TOLRNC = 2**-N (N=3)
!
      TOLRNC= 0.125D0

      CALL ZZGFSOLV ( T_UDQLT, GFSTEP,  GFREFN,  BAIL, &
                      GFBAIL,  CSTEP,   STEP,    START, &
                      FINISH,  TOLRNC,  RPT,     T_REPU, &
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
      CALL ZZCRITG ( CRIT )

      CALL CHCKSD ( 'TBEG', TBEG, '=',      CRIT+ TOLRNC/2.D0, 0.D0, OK)
      CALL CHCKSD ( 'TBEG', TEND, '=', 2.D0*CRIT+ TOLRNC/2.D0, 0.D0, OK)

!
!     We know the exact value for the number of report hits.
!
      CALL ZZNRPTG ( NRPT )
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
      CALL ZZCRITS( 93.D0)

!
!     Set TOLRNC to something we know will work for bisection.
!
      TOLRNC= 0.125D0

!
!     Use a test version of GFREFN to ensure a slower than
!     bisection convergence rate.
!
      CALL ZZGFSOLV ( T_UDQLT, GFSTEP,  T_REFN,  BAIL, &
                      GFBAIL,  CSTEP,   STEP,    START, &
                      FINISH,  TOLRNC,  RPT,     T_REPU, &
                      RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOCONVERG)', OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END



!
!     ZZGFSOLV test family specific utility code.
!
      SUBROUTINE T_ZZGFSOLV ( ET, IN_CON, VAL, IVBEG, IVEND, NVAL )

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      LOGICAL               IN_CON
      DOUBLE PRECISION      VAL
      DOUBLE PRECISION      IVBEG
      DOUBLE PRECISION      IVEND
      INTEGER               NVAL

      DOUBLE PRECISION      CRIT
      INTEGER               NRPT

      SAVE                  CRIT
      SAVE                  NRPT

      DATA                  NRPT        /0/

      CALL CHKIN  ( 'T_ZZGFSOLV'    )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_ZZGFSOLV'    )

      RETURN


!
!     A UDCOND test function. The function returns FALSE
!     for ET <= CRIT and ET > 2*CRIT.
!
!
      ENTRY T_UDQLT ( ET, IN_CON )

         IN_CON = .FALSE.

!
!        The function returns true after the CRIT value.
!
         IF ( ET .GT. CRIT ) THEN
            IN_CON = .TRUE.
         END IF

!
!        The function returns false after the 2*CRIT value.
!
         IF ( ET .GT. 2.D0*CRIT ) THEN
           IN_CON = .FALSE.
         END IF

      RETURN


!
!     Set the critical value used in the tests.
!
      ENTRY ZZCRITS( VAL )

         CRIT = VAL

      RETURN


!
!     Return the critical value used in the tests.TOL
!
      ENTRY ZZCRITG( VAL )

         VAL = CRIT

      RETURN

!
!     A UDREPU test function.
!
      ENTRY T_REPU( IVBEG, IVEND, ET )

!
!        Count the number of progress report calls.
!
         NRPT = NRPT + 1

      RETURN


!
!     Return the number of progress report calls then reset the
!     count variable to zero.
!
      ENTRY ZZNRPTG( NVAL )

         NVAL = NRPT
         NRPT = 0

      RETURN

!
!     Initialize the progress reporter count to zero.
!
      ENTRY ZZRPTI( )

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
      SUBROUTINE T_REFN ( T1, T2, S1, S2, T )
      IMPLICIT NONE

      DOUBLE PRECISION      T1
      DOUBLE PRECISION      T2
      LOGICAL               S1
      LOGICAL               S2
      DOUBLE PRECISION      T

      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      X
      DOUBLE PRECISION      EPSILON

      EPSILON = 0.999D0

      X = T1 * ( 1.D0 - EPSILON ) + ( T2 * EPSILON )

      T = BRCKTD ( X, T1, T2 )

      RETURN
      END


