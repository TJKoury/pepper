!$Procedure CURTIM (Current Time)

      SUBROUTINE CURTIM ( TIME )
      use pepper_legacy

! $ Abstract
!
!     Return a string giving the current date and time
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       TIME
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         TIME

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      TIME       O   A string containing the current date and time.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     TIME        is a string that contains the current date and
!                 time in the format YEAR-MON-DY HR:MN:SC
!
! $ Parameters
!
!      None.
!
! $ Files
!
!      None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This is a utility for creating time-stamps for inserting
!     into data products.  It is not intended to provide accurate
!     measurment of local time since local time is not necessarily
!     in step with the processor clock.  If you need the numeric
!     components, see the routine ZZCPUTIM.
!
!
! $ Examples
!
!     Suppose that you wish to insert into a data product the
!     system time at the time of creation of the product.  You
!     could call this routine to get the current time (in a string)
!     and then simply write that string into the data product.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-    SUPPORT Version 1.1.0, 11-SEP-2007 (EDW)
!
!        Replaced CPUTIM call with ZZCPUTIM call. Edited previous
!        Version entries to clarify CURTIM pedigree.
!
!-    SUPPORT Version 1.0.1, 03-MAY-1994 (WLT)
!
!        This is the configured version of the Command Loop
!        software as of May 4, 1994
!
!-    SUPPORT Version 1.0.0, 20-APR-1994 (WLT)
!
!-&

! $ Index_Entries
!
!     Get a string giving the current system time
!
!-&



      CHARACTER*(4)         YEAR
      CHARACTER*(3)         MONTH ( 12 )
      CHARACTER*(2)         DAY
      CHARACTER*(2)         HOUR
      CHARACTER*(2)         MIN
      CHARACTER*(2)         SEC

      DOUBLE PRECISION      TVEC ( 6 )
      INTEGER               IVEC ( 6 )
      INTEGER               I

      SAVE

      DATA                  MONTH / 'JAN', 'FEB', 'MAR', 'APR', &
                                    'MAY', 'JUN', 'JUL', 'AUG', &
                                    'SEP', 'OCT', 'NOV', 'DEC'  /

      CALL ZZCPUTIM( TVEC )

      DO I = 1,6
         IVEC(I) = INT(TVEC(I))
      END DO

      CALL INTSTR ( IVEC(1), YEAR  )
      CALL INTSTR ( IVEC(3), DAY   )
      CALL INTSTR ( IVEC(4), HOUR  )
      CALL INTSTR ( IVEC(5), MIN   )
      CALL INTSTR ( IVEC(6), SEC   )

      CALL RJUST  ( DAY,  DAY  )
      CALL RJUST  ( HOUR, HOUR )
      CALL RJUST  ( MIN,  MIN  )
      CALL RJUST  ( SEC,  SEC  )

      CALL REPLCH ( DAY,  ' ', '0', DAY  )
      CALL REPLCH ( HOUR, ' ', '0', HOUR )
      CALL REPLCH ( MIN,  ' ', '0', MIN  )
      CALL REPLCH ( SEC,  ' ', '0', SEC  )

      TIME = YEAR // '-' // MONTH(IVEC(2)) // '-' // DAY // ' ' &
      //     HOUR // ':' // MIN            // ':' // SEC

      RETURN
      END
