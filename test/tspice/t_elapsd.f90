!$Procedure T_ELAPSD ( Time program execution )

      SUBROUTINE T_ELAPSD ( REPORT, TITLE, KIND )


! $ Abstract
!
!     This routine helps timing the program execution.
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
!     TEST UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'zzddhman.inc'

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
