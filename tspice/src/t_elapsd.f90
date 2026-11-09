!$Procedure T_ELAPSD ( Time program execution )

      SUBROUTINE T_ELAPSD ( REPORT, TITLE, KIND )
      use pepper_inc_zzddhman


! $ Abstract
!
!     This routine helps timing the program execution.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST UTILITY
!
! $ Declarations

      IMPLICIT NONE


      LOGICAL               REPORT
      CHARACTER*(*)         TITLE
      CHARACTER*(*)         KIND

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     REPORT     I   Flag controlling printing timing info to STDOUT.
!     TITLE      I   Label printed for elapsed seconds.
!     KIND       I   Kind of timing info to print.
!
! $ Detailed_Input
!
!     REPORT     is the flag controlling whether to print (.TRUE.)
!                or not (.FALSE.) timing info to STDOUT.
!
!     TITLE      is the label printed of the left of the elapsed
!                seconds.
!
!     KIND       specifies the kind of elapsed seconds to be printed,
!                either the elapsed seconds from the first call to this
!                routine ('TOTAL') or the elapsed seconds since the
!                last call to this routine (any other value).
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
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     If REPORT = .TRUE. this routine prints either the total or
!     running elapsed seconds to STDOUT.
!
! $ Examples
!
!     See F_DDHFTSIZE or F_ZZHSC.
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
!     B.V. Semenov     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 02-AUG-2013 (BVS)
!
!
!-&

!
!     SPICELIB functions.
!
      LOGICAL               EQSTR

!
!     Local variables
!
      LOGICAL               FIRST
      DOUBLE PRECISION      TVEC   ( 6 )
      DOUBLE PRECISION      SECS
      DOUBLE PRECISION      SECS0
      DOUBLE PRECISION      SECSP
      CHARACTER*(50)        HLINE
      CHARACTER*(32)        HWORD

!
!     Save some.
!
      SAVE                  FIRST
      SAVE                  SECS0
      SAVE                  SECSP

!
!     Initial values.
!
      DATA                  FIRST   / .TRUE. /

!
!     Get time. Compute seconds from the start of the day.
!
      CALL CPUTIM( TVEC )
      SECS  =  TVEC(4)*3600.D0 + TVEC(5)*60.D0 + TVEC(6)

!
!     Set initial time.
!
      IF ( FIRST ) THEN
         SECS0 = SECS
         SECSP = SECS
         FIRST = .FALSE.
      END IF

!
!     Report total elapsed time or running elapsed time, if requeated.
!
      IF ( REPORT ) THEN

         HLINE = TITLE
         CALL RJUST( HLINE, HLINE )

         IF ( EQSTR( KIND, 'TOTAL' ) ) THEN
            CALL DPFMT   ( SECS - SECS0, 'xxxxxx.xxx', HWORD )
            CALL TOSTDO  ( HLINE // ': elapsed ' // HWORD )
         ELSE
            CALL DPFMT   ( SECS - SECSP, 'xxxxxx.xxx', HWORD )
            CALL TOSTDO  ( HLINE // ': elapsed ' // HWORD )
         END IF

      END IF

      SECSP = SECS

      RETURN

      END
