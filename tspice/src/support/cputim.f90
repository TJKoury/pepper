!$Procedure CPUTIM ( CPU Time )

      SUBROUTINE CPUTIM ( TVEC )

! $ Abstract
!
!     Fetch the current CPU date and time and store the result
!     as a double precision 6-vector.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TIME
!     UTILITY
!
! $ Declarations

      DOUBLE PRECISION      TVEC ( 6 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TVEC       O   contains year, month, day, hours, minutes, seconds
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     TVEC       is a 6-vector containing the current system time.
!                The various components have the following meaning
!
!                   TVEC(1)  --- current calendar year
!                   TVEC(2)  --- current month
!                   TVEC(3)  --- current day of month
!                   TVEC(4)  --- current hour. Hours have a range from
!                                0 to 23.  0 corresponds to system
!                                midnight.
!                   TVEC(5)  --- current minutes
!                   TVEC(6)  --- current seconds and fraction of a
!                                second (provided the system clock
!                                has sufficiently fine granularity
!                                to provide greater precision).
!
!                The first 5 components will be double precision
!                integers.  (They truncate without change.)
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
!     This routine returns the components of the current date and
!     time as determined by the system clock.
!
!
! $ Examples
!
!     Example 1.
!
!     The following routine illustrates how you might use CPUTIM
!     to generate a "time stamp" that might be used to tag data
!     you plan to write to a file.
!
!           SUBROUTINE TSTAMP ( STAMP )
!
!           CHARACTER*(15)        STAMP
!
!           DOUBLE PRECISION      TVEC ( 6 )
!
!     C
!     C     First fetch the current system time.
!     C
!           CALL CPUTIM ( TVEC )
!
!     C
!     C     now form a time stamp of the form YYYYYMMDDhhmmss
!     C
!           CALL DPFMT ( TVEC(1), '0YYYY', STAMP(1:5)   )
!           CALL DPFMT ( TVEC(2), '0M',    STAMP(6:7)   )
!           CALL DPFMT ( TVEC(3), '0D',    STAMP(8:9)   )
!           CALL DPFMT ( TVEC(4), '0h',    STAMP(10:11) )
!           CALL DPFMT ( TVEC(5), '0m',    STAMP(12:13) )
!           CALL DPFMT ( TVEC(6), '0s',    STAMP(14:15) )
!
!           RETURN
!
!     Example 2.
!
!     The following code illustrates how you might use this routine
!     to perform a crude estimate of the running time of the
!     SPICELIB routine VDOT. (This assumes that the program runs
!     during a single calendar day and that there is no competition
!     between users for system resources.)
!
!           DOUBLE PRECISION      VDOT
!
!           DOUBLE PRECISION      AVE
!           DOUBLE PRECISION      SINCE0( 2    )
!           DOUBLE PRECISION      TVEC  ( 6, 3 )
!           DOUBLE PRECISION      V1    ( 3    )
!           DOUBLE PRECISION      V2    ( 3    )
!           DOUBLE PRECISION      X
!
!           INTEGER               I
!           INTEGER               TRIALS
!           PARAMETER           ( TRIALS = 100000 )
!
!     C
!     C     Give the vectors some values (these seem as good as
!     C     anything else that comes to mind).
!     C
!           V1(1) = 1.0D0
!           V1(2) = 2.0D0
!           V1(3) = 3.0D0
!
!           V2(1) = 10.0D0
!           V2(2) = 20.0D0
!           V3(3) = 30.0D0
!
!     C
!     C     Perform the loop twice, once with one call to VDOT, the
!     C     second with two calls to VDOT.
!     C     The first will require
!     C
!     C        LOOP_OVERHEAD + TRIALS*TIME_FOR_VDOT
!     C
!     C     The second will require
!     C
!     C        LOOP_OVERHEAD + 2*TRIALS*TIME_FOR_VDOT
!     C
!     C     The difference of the two, will give us
!     C
!     C        TRIALS*TIME_FOR_VDOT
!     C
!
!     C
!     C     get the current system time.
!     C
!           CALL CPUTIM ( TVEC(1,1) )
!
!           DO I = 1, TRIALS
!              X = VDOT( V1, V2 )
!           END DO
!
!     C
!     C     Get the time after the first pass.
!     C
!           CALL CPUTIM ( TVEC(1,2)
!
!           DO I = 1, TRIALS
!              X = VDOT( V1, V2 )
!              X = VDOT( V1, V2 )
!           END DO
!
!      C
!      C    Get the time after the second pass.
!      C
!           CALL CPUTIM ( TVEC(1,3)
!
!
!      C
!      C    Now compute seconds past midnight at each clock reading.
!      C
!           DO I = 1, 3
!
!              SINCE0(I) = TVEC(4,I) * 3600.0D0
!          .             + TVEC(5,I) *   60.0D0
!          .             + TVEC(6,I)
!           END DO
!
!      C
!      C    The time for the first  pass is SINCE0(2) - SINCE0(1)
!      C    The time for the second pass is SINCE0(3) - SINCE0(2)
!      C
!      C    The difference between these divided by the number of
!      C    trials is the average running time.
!      C
!           AVE = (SINCE0(3) - 2*SINCE0(2) - SINCE0(1)) / DBLE(TRIALS)
!
!           WRITE (*,*) 'The average running time for VDOT is ', AVE
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!   None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     K.R. Gehringer (JPL)
!     H.A. Neilan    (JPL)
!     M.J. Spencer   (JPL)
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    SPICELIB Version  1.0.0, 13-FEB-2008 (EDW)
!
!        This routine calls the ZZCPUTIM routine in SPICELIB,
!        performing no other operation.
!
!-&

! $ Index_Entries
!
!     get system date and time
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CPUTIM' )
      END IF

!
!     Get the date and time.
!
      CALL ZZCPUTIM( TVEC )

!
!     That's it.
!
      CALL CHKOUT ( 'CPUTIM' )
      RETURN

      END
