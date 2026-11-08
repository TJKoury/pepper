C$Procedure F_GFUDB ( GFUDB family tests )

      SUBROUTINE F_GFUDB ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        GFUDB
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
C     TEST
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'

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
C     This routine tests the higher-level SPICELIB geometry
C     routine GFUDB.
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
C-    TSPICE Version 1.1.0, 28-JAN-2017 (EDW)
C
C        Modified GFSTOL test case to correspond to change
C        in ZZGFSOLVX.
C
C-    TSPICE Version 1.0.0, 08-APR-2014 (EDW)
C
C-&

C
C     SPICELIB functions
C
      INTEGER               WNCARD
      DOUBLE PRECISION      SPD


C
C     Local parameters
C
      EXTERNAL              GFB
      EXTERNAL              UDF

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 50 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Local variables
C
      DOUBLE PRECISION      CNFINE ( LBCELL : 2      )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      LHS    ( 3 )
      DOUBLE PRECISION      RHS    ( 3 )
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      END


      INTEGER               BADSIZ ( 3 )
      INTEGER               I
      INTEGER               J
      INTEGER               COUNT

      CHARACTER*(LNSIZE)    MSG1
      CHARACTER*(LNSIZE)    MSG2
      CHARACTER*(LNSIZE)    TITLE

C
C     Save everything
C
      SAVE


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFUDB' )


      BADSIZ(1) = 0
      BADSIZ(2) = 1
      BADSIZ(3) = 3


C
C     The event boundaries as defined in GFB.
C
      LHS(1) = 20.D0
      LHS(2) = 55.D0
      LHS(3) = 92.D0

      RHS(1) = 30.D0
      RHS(2) = 57.D0
      RHS(3) = 100.D0




C
C     Error cases
C

C
C     Case 1:
C
C     Confirm bad window sizes cause the expected error.
C

      STEP   = SPD()

      CALL SCARDD ( 0, RESULT )
      CALL SCARDD ( 0, CNFINE )

      MSG1 = 'Invalid result window size #'

      DO I = 1, 3

         CALL REPMI ( MSG1, '#', BADSIZ(I), MSG1 )

         CALL TCASE ( MSG1 )

         CALL SSIZED ( BADSIZ(I), RESULT )

         CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )

      END DO

C
C     Setp appropriate sizes for the windows.
C
      CALL SSIZED ( MAXWIN,      RESULT )
      CALL SSIZED ( 2,           CNFINE )




C
C     Case 2:
C
C     Confirm non-positive step causes the expected error.
C
      CALL TCASE ( 'Non-positive step size.' )

      STEP = -1.D0
      CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )

      STEP = 0.D0
      CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )




C
C     Standard cases.
C

C
C     Case 3:
C
C     Test output from use of GFB
C

C
C     Store the time bounds of our search interval in
C     the confinement window.
C
      CALL WNINSD ( -5.D0, 105.D0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      STEP = 1.9D0

      CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     We expect 3 intervals in RESULT.
C
      CALL CHCKSI ( 'WNCARD( RESULT )',
     .               WNCARD( RESULT ), '=', 3, 0,  OK )

         IF ( OK ) THEN

         DO J=1, WNCARD( RESULT )

C
C           Fetch the endpoints of the Jth interval of the result
C           window. The window interval values should equal the
C           expected values (LHS, RHS) to within CNVTOL.
C
            CALL WNFETD ( RESULT, J, LEFT, RIGHT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            MSG1 = 'GFB use output, LHS #'
            CALL REPMI ( MSG1, '#', J, MSG1 )

            CALL TCASE ( MSG1 )
            CALL CHCKSD ( MSG1, LEFT, '~', LHS(J), CNVTOL, OK )


            MSG2 = 'GFB use output, RHS #'
            CALL REPMI ( MSG2, '#', J, MSG2 )

            CALL TCASE ( MSG2 )
            CALL CHCKSD ( MSG2, RIGHT, '~', RHS(J), CNVTOL, OK )


         END DO

      END IF




C
C     Case 4
C
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


C
C     Re-run a valid search after using GFSTOL to set the convergence
C     tolerance to a value that should cause a numerical error signal.
C

      STEP = 1.D0

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Reset tol.
C

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     The values in the time window should not match
C     as the search used different tolerances. Check
C     the first value in the first interval.
C
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )


C
C     Reset the convergence tolerance.
C
      CALL GFSTOL ( CNVTOL )


      RETURN
      END




C---    The user defined functions required by GFUDB.
C
C      gfb    for udfunb
C

C$Procedure GFB ( Time dependent boolean function )

      SUBROUTINE GFB ( UDFUNS, T, XBOOL )

C$ Abstract
C
C     User defined boolean quantity function.
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
C     None.
C
C$ Declarations

      IMPLICIT NONE

      EXTERNAL                  UDFUNS
      DOUBLE PRECISION          T
      LOGICAL                   XBOOL

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
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
C     E. D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 02-NOV-2011 (EDW)
C
C-&

C
C     Return false unless otherwise assigned.
C
      XBOOL = .FALSE.

C
C     An arbitrary boolean function with known boundaries
C     at step event.
C
C           -----           -           ----
C          |     |         | |         |    |
C     -----|     |--- ~ ---| |--- ~ ---|    |----
C         20     30       55 57       92   100
C

      IF (      (T .GT. 20.D0) .AND. (T .LT. 30.D0) ) THEN

         XBOOL = .TRUE.

      ELSE IF ( (T .GT. 55.D0) .AND. (T .LT. 57.D0) ) THEN

         XBOOL = .TRUE.

      ELSE IF ( (T .GT. 92.D0) .AND. (T .LT. 100.D0) ) THEN

         XBOOL = .TRUE.

      END IF

      RETURN
      END


C23456789012345678901234567890123456789012345678901234567890123456789012
