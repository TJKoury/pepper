!$Procedure      CHCKAI ( Check an array of integers )

      SUBROUTINE CHCKAI ( NAME, ARRAY, COMP, EXP, SIZE,  OK )

! $ Abstract
!
!     Check the  values in two integer arrays..
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       TESTING
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         NAME
      INTEGER               ARRAY ( * )
      CHARACTER*(*)         COMP
      INTEGER               EXP   ( * )
      INTEGER               SIZE
      LOGICAL               OK


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      NAME       I   the name of the array to be examined.
!      ARRAY      I   the actual array
!      COMP       I   the kind of comparison to perform.
!      EXP        I   the comparison values for the array
!      SIZE       I   the size of the input array
!      OK         O   TRUE if the test passes, FALSE otherwise..
!
! $ Detailed_Input
!
!     NAME        is the string used to give the name of an array.
!
!     ARRAY       is the actual d.p. array to be examined
!
!     COMP        a string giving the kind of comparison to perform:
!
!                    =    ---   check for strict equality
!
!     EXP         an expected values or bounds on the values in ARRAY.
!
!
! $ Detailed_Output
!
!     OK         if the check of the input array is successful then
!                OK is given the value TRUE.  Otherwise OK is given the
!                value FALSE and a diagnostic message is sent to the
!                test logger.
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!      None.
!
! $ Particulars
!
!     This routine handles a wide variety of comparisons between
!     double precision arrays.
!
! $ Examples
!
!     Suppose that you have just made a call to a subroutine that
!     you wish to test (call the routine SPUD) and you would like
!     to test an output d.p. against an expected value and verify that
!     the relative difference is less than some value.  Using
!     this routine you can automatically have the test result logged
!     in via the testing utilities.
!
!        CALL SPUD   (  INPUT,   OUTPUT )
!        CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', EXPECT, OK )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!      None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     F.S. Turner     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    Testing Utility Version 1.2.0, 05-AUG-2013 (BVS)
!
!        Tweaked logic to do only necessary output message construction
!        operations to speed-up execution.
!
!-    Testing Utility Version 1.1.0, 10-MAY-2001 (FST)
!
!        OK is now properly set before returning control to the
!        caller in all cases.
!
!-    Testing Utility Version 1.0.0, 7-NOV-1994 (WLT)
!
!
!-&
!
!     SPICELIB functions
!
      INTEGER               RTRIM
!
!     Testing Utility Functions
!
      LOGICAL               VERBOS
!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 800 )


      CHARACTER*(LNSIZE)    MESSGE

      INTEGER               I


      LOGICAL               FAIL

      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )

      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD

      CALL TSTSTY ( GOOD, BAD )
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ', &
                    'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )

!
!     Set placeholder message.
!
      MESSGE = 'This is a placeholder message that should have never '// &
               'been displayed. It indicates a CHCKAI bug.'


      FAIL = .FALSE.

      IF      ( COMP .EQ. '='  ) THEN

         DO I = 1, SIZE

            FAIL = ARRAY(I) .NE. EXP(I)

            IF ( FAIL ) THEN
               MESSGE = 'Value # of array # was not the value ' &
               //       'expected. /cr(3:)/cr ' &
               //       'The value was:         # /cr' &
               //       'the expected value was #./cr' &
               //       'The difference between these is: # . '
               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMI ( MESSGE, '#', ARRAY(I),            MESSGE )
               CALL REPMI ( MESSGE, '#', EXP(I),              MESSGE )
               CALL REPMI ( MESSGE, '#', ARRAY(I)-EXP(I),     MESSGE )
               CALL TSTLOG( ' ',    FAIL )
               CALL TSTLOG( MESSGE, FAIL )
               CALL TSTLGS ( GOOD, BAD )
               OK = .NOT. FAIL
               RETURN
            END IF
         END DO

      ELSE

         FAIL = .TRUE.

         MESSGE = 'The comparison "#" is not recognized. '
         CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )

      END IF

      IF ( .NOT. FAIL ) THEN

         IF ( VERBOS() ) THEN

            MESSGE = 'The comparison /cr/cr(3:)''# # EXPECTED'' ' &
         //          '/cr/cr(-3:) ' &
         //          'was satisfied.'

            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )

            CALL TSTLOG ( ' ',    FAIL )
            CALL TSTLOG ( MESSGE, FAIL )

         END IF

      ELSE

         CALL TSTLOG ( ' ',    FAIL )
         CALL TSTLOG ( MESSGE, FAIL )

      END IF

      CALL TSTLGS ( GOOD, BAD )
      OK = .NOT. FAIL

      RETURN
      END
