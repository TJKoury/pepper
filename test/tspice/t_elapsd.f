C$Procedure T_ELAPSD ( Time program execution )
 
      SUBROUTINE T_ELAPSD ( REPORT, TITLE, KIND )

 
C$ Abstract
C
C     This routine helps timing the program execution.
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
C     TEST UTILITY
C
C$ Declarations
 
      IMPLICIT NONE

      INCLUDE               'zzddhman.inc'

      LOGICAL               REPORT
      CHARACTER*(*)         TITLE
      CHARACTER*(*)         KIND
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     REPORT     I   Flag controlling printing timing info to STDOUT.
C     TITLE      I   Label printed for elapsed seconds.
C     KIND       I   Kind of timing info to print.
C
C$ Detailed_Input
C
C     REPORT     is the flag controlling whether to print (.TRUE.) 
C                or not (.FALSE.) timing info to STDOUT.
C
C     TITLE      is the label printed of the left of the elapsed
C                seconds.
C
C     KIND       specifies the kind of elapsed seconds to be printed,
C                either the elapsed seconds from the first call to this
C                routine ('TOTAL') or the elapsed seconds since the
C                last call to this routine (any other value).
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
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     If REPORT = .TRUE. this routine prints either the total or
C     running elapsed seconds to STDOUT.
C
C$ Examples
C
C     See F_DDHFTSIZE or F_ZZHSC.
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
C     B.V. Semenov     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 02-AUG-2013 (BVS)
C
C
C-&

C
C     SPICELIB functions.
C
      LOGICAL               EQSTR

C
C     Local variables
C
      LOGICAL               FIRST
      DOUBLE PRECISION      TVEC   ( 6 )
      DOUBLE PRECISION      SECS
      DOUBLE PRECISION      SECS0
      DOUBLE PRECISION      SECSP
      CHARACTER*(50)        HLINE
      CHARACTER*(32)        HWORD

C
C     Save some.
C
      SAVE                  FIRST
      SAVE                  SECS0
      SAVE                  SECSP

C
C     Initial values.
C
      DATA                  FIRST   / .TRUE. /

C
C     Get time. Compute seconds from the start of the day.
C
      CALL CPUTIM( TVEC )
      SECS  =  TVEC(4)*3600.D0 + TVEC(5)*60.D0 + TVEC(6)

C
C     Set initial time.
C
      IF ( FIRST ) THEN
         SECS0 = SECS
         SECSP = SECS
         FIRST = .FALSE.
      END IF

C
C     Report total elapsed time or running elapsed time, if requeated.
C
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
