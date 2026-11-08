C$Procedure F_ZZGFSOLVX ( ZZGFSOLV family tests )

      SUBROUTINE F_ZZGFSOLVX ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routines
C
C        ZZGFSOLVX
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
C     This family tests the GF event finder ZZGFSOLVX. Code
C     based on the original F_ZZGFSOLVX routine. No changes to test
C     logic.
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
C-    TSPICE Version 1.2.0, 14-OCT-2021 (BVS)
C
C        Bug fix: fixed routine name in CHKIN/CHKOUT calls
C        in entry T_SOLVX (T_ZZGFSOLVX -> T_SOLVX).
C
C-    TSPICE Version 1.1.0, 11-FEB-2017 (EDW) (NJB)
C
C        Eliminated small TOLRNC test cases to correspond to change
C        in ZZGFSOLVX.
C
C-    TSPICE Version 1.0.0, 27-APR-2012 (EDW)
C
C        Code based on F_ZZGFSOLV.
C-&

C$ Index_Entries
C
C   None.
C
C-&

C
C     SPICELIB functions
C
      INTEGER               WNCARD

      LOGICAL               GFBAIL

C
C     External routines
C
      EXTERNAL              GFBAIL
      EXTERNAL              GFREFN
      EXTERNAL              GFREPU
      EXTERNAL              GFSTEP
      EXTERNAL              T_UDQLTX
      EXTERNAL              T_REPUX
      EXTERNAL              T_REFNX
      EXTERNAL              UDFUNCX

C
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 100 )

C
C     Local Variables
C
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

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFSOLVX' )

C
C     Set the progress report test hit counter to zero.
C
      CALL ZZRPTIX()


C
C     Case 1
C
      CALL TCASE ( 'TOLRNC = 0' )

      BAIL  = .FALSE.
      CSTEP = .TRUE.
      RPT   = .FALSE.
      STEP  = 1.D0
      START = 0.D0
      FINISH= 10.D0
      TOLRNC= 0.D0

      CALL ZZGFSOLVX( UDFUNCX, T_UDQLTX, GFSTEP,  GFREFN,  BAIL,
     .                GFBAIL,   CSTEP,   STEP,    START,
     .                FINISH,  TOLRNC,  RPT,     GFREPU,
     .                RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )


C
C     Case 2
C
      CALL TCASE ( 'START > FINISH' )

      BAIL  = .FALSE.
      CSTEP = .TRUE.
      RPT   = .FALSE.
      STEP  = 1.D0
      START = 10.D0
      FINISH= 0.D0
      TOLRNC= 1.D0

      CALL ZZGFSOLVX( UDFUNCX, T_UDQLTX, GFSTEP,  GFREFN,  BAIL,
     .                GFBAIL,   CSTEP,   STEP,    START,
     .                FINISH,  TOLRNC,  RPT,     GFREPU,
     .                RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADTIMECASE)', OK )



C
C     Case 3
C
      CALL TCASE ( 'Step function, one interval, one critical value.' )

C
C     Set a search to key on the critical value defined in
C     T_ZZGFSOLVX. With a STEP size of one, and a critical value
C     a double precision representation of an integer, the
C     RESULT window returns [CRIT + TOLRNC/2, FINISH].
C
C                            ------------
C                            |
C           -----------------|
C

      CALL SSIZED ( MAXWIN, RESULT )
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BAIL  = .FALSE.
      CSTEP = .TRUE.
      RPT   = .TRUE.
      STEP  = 1.D0
      START = 0.D0
      FINISH= 100.D0

C
C     Set a value strictly between START and FINISH.
C
      CALL ZZCRITSX( 93.D0)

C
C     Using bisection for refinement; set TOLRNC as a power of 2.
C
C     TOLRNC = 2**-N (N=3)
C
      TOLRNC= 0.125D0

      CALL ZZGFSOLVX( UDFUNCX, T_UDQLTX, GFSTEP,  GFREFN,  BAIL,
     .                GFBAIL,  CSTEP,   STEP,    START,
     .                FINISH,  TOLRNC,  RPT,     T_REPUX,
     .                RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect one root.
C
      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL WNFETD ( RESULT, N, TBEG, TEND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We know the exact values for the expected interval endpoints.
C
      CALL ZZCRITGX( CRIT )

      CALL CHCKSD ( 'TBEG', TBEG, '=', CRIT+ TOLRNC/2.D0, 0.D0, OK )
      CALL CHCKSD ( 'TBEG', TEND, '=', FINISH,            0.D0, OK )

C
C     We know the exact value for the number of report hits.
C
      CALL ZZNRPTGX( NRPT )

      CALL CHCKSI ( 'NRPT', NRPT, '=', 101, 0, OK)


C
C     Case 4
C
      CALL TCASE ( 'Step function, one interval, two critical values.' )

C
C                            -----------------
C                            |               |
C           -----------------|               |--
C                           CRIT           2*CRIT
C
      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BAIL  = .FALSE.
      CSTEP = .TRUE.
      RPT   = .TRUE.
      STEP  = 1.D0
      START = 0.D0
      FINISH= 200.D0

C
C     Set a value strictly between START and FINISH with
C     2*CRIT + TOLRNC/2.D0 < FINISH.
C
      CALL ZZCRITSX ( 93.D0)

C
C     Using bisection for refinement; set TOLRNC as a power of 2.
C
C     TOLRNC = 2**-N (N=3)
C
      TOLRNC= 0.125D0

      CALL ZZGFSOLVX( UDFUNCX, T_UDQLTX, GFSTEP,  GFREFN,  BAIL,
     .                GFBAIL,  CSTEP,   STEP,    START,
     .                FINISH,  TOLRNC,  RPT,     T_REPUX,
     .                RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect one root.
C
      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL WNFETD ( RESULT, N, TBEG, TEND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We know the exact values for the expected interval endpoints.
C
      CALL ZZCRITGX ( CRIT )

      CALL CHCKSD ( 'TBEG', TBEG, '=',      CRIT+ TOLRNC/2.D0, 0.D0, OK)
      CALL CHCKSD ( 'TBEG', TEND, '=', 2.D0*CRIT+ TOLRNC/2.D0, 0.D0, OK)

C
C     We know the exact value for the number of report hits.
C
      CALL ZZNRPTGX ( NRPT )
      CALL CHCKSI ( 'NRPT', NRPT, '=', 200, 0, OK)



C
C     Case 5
C
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

C
C     Set a value strictly between START and FINISH.
C
      CALL ZZCRITSX( 93.D0)

C
C     Set TOLRNC to something we know will work for bisection.
C
      TOLRNC= 0.125D0

C
C     Use a test version of GFREFN to ensure a slower than
C     bisection convergence rate.
C
      CALL ZZGFSOLVX( UDFUNCX, T_UDQLTX, GFSTEP,  T_REFNX,  BAIL,
     .                GFBAIL,  CSTEP,   STEP,    START,
     .                FINISH,  TOLRNC,  RPT,     T_REPUX,
     .                RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOCONVERG)', OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END



C$Procedure T_SOLVX ( Test utilities )

       SUBROUTINE T_SOLVX ( UDFUNCX, ET,   IN_CON, VAL,
     .                      IVBEG,  IVEND, NVAL )

C$ Abstract
C
C     ZZGFSOLVX test family specific utility code.
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
C   None.
C
C$ Keywords
C
C   None.
C
C$ Declarations

      IMPLICIT NONE

      EXTERNAL              UDFUNCX

      DOUBLE PRECISION      ET
      LOGICAL               IN_CON
      DOUBLE PRECISION      VAL
      DOUBLE PRECISION      IVBEG
      DOUBLE PRECISION      IVEND
      INTEGER               NVAL

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C$ Detailed_Input
C
C   None.
C
C$ Detailed_Output
C
C   None.
C
C$ Parameters
C
C   None.
C
C$ Exceptions
C
C   None.
C
C$ Files
C
C   None.
C
C$ Particulars
C
C   None.
C
C$ Examples
C
C   None.
C
C$ Restrictions
C
C   None.
C
C$ Literature_References
C
C   None.
C
C$ Author_and_Institution
C
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 27-APR-2012 (EDW)
C
C-&

C$ Index_Entries
C
C   None.
C
C-&

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


C
C     A UDCOND test function. The function returns FALSE
C     for ET <= CRIT and ET > 2*CRIT.
C
C
      ENTRY T_UDQLTX ( UDFUNCX, ET, IN_CON )

         IN_CON = .FALSE.

         CALL UDFUNCX ( ET, X )

C
C        The function returns true after the CRIT value.
C
         IF ( X .GT. CRIT ) THEN
            IN_CON = .TRUE.
         END IF

C
C        The function returns false after the 2*CRIT value.
C
         IF ( X .GT. 2.D0*CRIT ) THEN
           IN_CON = .FALSE.
         END IF

      RETURN


C
C     Set the critical value used in the tests.
C
      ENTRY ZZCRITSX( VAL )

         CRIT = VAL

      RETURN


C
C     Return the critical value used in the tests.
C
      ENTRY ZZCRITGX( VAL )

         VAL = CRIT

      RETURN

C
C     A UDREPU test function.
C
      ENTRY T_REPUX( IVBEG, IVEND, ET )

C
C        Count the number of progress report calls.
C
         NRPT = NRPT + 1

      RETURN


C
C     Return the number of progress report calls then reset the
C     count variable to zero.
C
      ENTRY ZZNRPTGX( NVAL )

         NVAL = NRPT
         NRPT = 0

      RETURN


      ENTRY ZZRPTIX( )

         NRPT = 0

      RETURN

      END


C
C     Test version of GFREFN. The EPSILON value causes
C     an increase in the number of refinement steps
C     required to converge as compared to bisection.
C
C     The default GFREFN uses an EPSILON value 0.5.
C
      SUBROUTINE T_REFNX ( T1, T2, S1, S2, T )
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


C$Procedure UDFUNCX ( Test routine )

      SUBROUTINE UDFUNCX ( ET, X )

C$ Abstract
C
C     GF test function; echo input as output.
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
C   None.
C
C$ Keywords
C
C   None.
C
C$ Declarations

      IMPLICIT              NONE

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      X

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C$ Detailed_Input
C
C   None.
C
C$ Detailed_Output
C
C   None.
C
C$ Parameters
C
C   None.
C
C$ Exceptions
C
C   None.
C
C$ Files
C
C   None.
C
C$ Particulars
C
C   None.
C
C$ Examples
C
C   None.
C
C$ Restrictions
C
C   None.
C
C$ Literature_References
C
C   None.
C
C$ Author_and_Institution
C
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 27-APR-2012 (EDW)
C
C-&

C$ Index_Entries
C
C   None.
C
C-&

      X = ET

      RETURN
      END

