!$Procedure F_GFUDB ( GFUDB family tests )

      SUBROUTINE F_GFUDB ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        GFUDB
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
!     TEST
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'

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
!     This routine tests the higher-level SPICELIB geometry
!     routine GFUDB.
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
!-    TSPICE Version 1.1.0, 28-JAN-2017 (EDW)
!
!        Modified GFSTOL test case to correspond to change
!        in ZZGFSOLVX.
!
!-    TSPICE Version 1.0.0, 08-APR-2014 (EDW)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               WNCARD
      DOUBLE PRECISION      SPD


!
!     Local parameters
!
      EXTERNAL              GFB
      EXTERNAL              UDF

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 50 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Local variables
!
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

!
!     Save everything
!
      SAVE


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFUDB' )


      BADSIZ(1) = 0
      BADSIZ(2) = 1
      BADSIZ(3) = 3


!
!     The event boundaries as defined in GFB.
!
      LHS(1) = 20.D0
      LHS(2) = 55.D0
      LHS(3) = 92.D0

      RHS(1) = 30.D0
      RHS(2) = 57.D0
      RHS(3) = 100.D0




!
!     Error cases
!

!
!     Case 1:
!
!     Confirm bad window sizes cause the expected error.
!

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

!
!     Setp appropriate sizes for the windows.
!
      CALL SSIZED ( MAXWIN,      RESULT )
      CALL SSIZED ( 2,           CNFINE )




!
!     Case 2:
!
!     Confirm non-positive step causes the expected error.
!
      CALL TCASE ( 'Non-positive step size.' )

      STEP = -1.D0
      CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )

      STEP = 0.D0
      CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )




!
!     Standard cases.
!

!
!     Case 3:
!
!     Test output from use of GFB
!

!
!     Store the time bounds of our search interval in
!     the confinement window.
!
      CALL WNINSD ( -5.D0, 105.D0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      STEP = 1.9D0

      CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     We expect 3 intervals in RESULT.
!
      CALL CHCKSI ( 'WNCARD( RESULT )', &
                     WNCARD( RESULT ), '=', 3, 0,  OK )

         IF ( OK ) THEN

         DO J=1, WNCARD( RESULT )

!
!           Fetch the endpoints of the Jth interval of the result
!           window. The window interval values should equal the
!           expected values (LHS, RHS) to within CNVTOL.
!
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




!
!     Case 4
!
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


!
!     Re-run a valid search after using GFSTOL to set the convergence
!     tolerance to a value that should cause a numerical error signal.
!

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

!
!     Reset tol.
!

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     The values in the time window should not match
!     as the search used different tolerances. Check
!     the first value in the first interval.
!
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )


!
!     Reset the convergence tolerance.
!
      CALL GFSTOL ( CNVTOL )


      RETURN
      END




!---    The user defined functions required by GFUDB.
!
!      gfb    for udfunb
!

!$Procedure GFB ( Time dependent boolean function )

      SUBROUTINE GFB ( UDFUNS, T, XBOOL )

! $ Abstract
!
!     User defined boolean quantity function.
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
!     None.
!
! $ Declarations

      IMPLICIT NONE

      EXTERNAL                  UDFUNS
      DOUBLE PRECISION          T
      LOGICAL                   XBOOL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
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
!     E. D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 02-NOV-2011 (EDW)
!
!-&

!
!     Return false unless otherwise assigned.
!
      XBOOL = .FALSE.

!
!     An arbitrary boolean function with known boundaries
!     at step event.
!
!           -----           -           ----
!          |     |         | |         |    |
!     -----|     |--- ~ ---| |--- ~ ---|    |----
!         20     30       55 57       92   100
!

      IF (      (T .GT. 20.D0) .AND. (T .LT. 30.D0) ) THEN

         XBOOL = .TRUE.

      ELSE IF ( (T .GT. 55.D0) .AND. (T .LT. 57.D0) ) THEN

         XBOOL = .TRUE.

      ELSE IF ( (T .GT. 92.D0) .AND. (T .LT. 100.D0) ) THEN

         XBOOL = .TRUE.

      END IF

      RETURN
      END


!23456789012345678901234567890123456789012345678901234567890123456789012
