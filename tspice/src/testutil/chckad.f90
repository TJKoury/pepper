!$Procedure CHCKAD ( Check an array of double precision values )

      SUBROUTINE CHCKAD ( NAME, ARRAY, COMP, EXP, SIZE,  TOL, OK )
      use pepper_legacy

! $ Abstract
!
!     Check the ordering of values in a d.p. array.
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
      DOUBLE PRECISION      ARRAY ( * )
      CHARACTER*(*)         COMP
      DOUBLE PRECISION      EXP   ( * )
      INTEGER               SIZE
      DOUBLE PRECISION      TOL
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
!      TOL        I   the tolerance allowed in comparing.
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
!                    ~    ---   check for ARRAY(I) ~  EXP(I) for each I.
!                               This checks that the difference
!                               |ARRAY(I) - EXP(I)| <= TOL for all I.
!
!                    ~/   ---   check for ARRAY(I) ~/ EXP(I) ( Relative
!                               difference between ARRAY(I) and EXP(I)
!                               .LE. TOL for all I.
!
!                    ||   ---   check that the "angle" between ARRAY
!                               and EXP is within TOL of zero.
!
!                    ~~   ---   check that the L2 distance between
!                               ARRAY and EXP is within TOL of zero.
!
!                    |_   ---   check that the angle between ARRAY
!                               and EXP is within TOL of pi/2 radians
!
!                    ~~/  ---   check that the vector relative
!                               difference between ARRAY and EXP is
!                               within TOL of zero.
!
!     EXP         an expected values or bounds on the values in ARRAY.
!
!
!     TOL        is a "tolerance" to use when checking for VAL to
!                be nearly the same as EXP.
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
!        CALL CHCKSD ( 'OUTPUT', OUTPUT, '~/', EXPECT, 1.0D-12, OK )
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
!     E.W. Wright     (JPL)
!     B.V. Semenov    (JPL)
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    Testing Utility Version 1.3.0, 23-APR-2014 (BVS)(NJB)
!
!        Added checks for expected or actual values equal to NaN.
!
!        Tweaked logic to do only necessary output message construction
!        operations to speed-up execution.
!
!-    Testing Utility Version 1.2.0, 20-OCT-2006 (EDW)
!
!        Corrected logic error in the ~~/ test block. Replaced the
!        line
!
!           IF ( SIZE1 .EQ. SIZE2 ) THEN
!
!        with
!
!           IF ( (SIZE1 .EQ. 0.D0 ) .AND. (SIZE2 .EQ. 0.D0) ) THEN
!
!-    Testing Utility Version 1.1.0, 10-MAY-2001 (FST)
!
!        OK is now properly set in all cases before returning
!        control to the caller.
!
!-    Testing Utility Version 1.0.0, 7-NOV-1994 (WLT)
!
!-&

!
!     SPICELIB functions
!

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

      DOUBLE PRECISION      REL
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      DEFECT
      DOUBLE PRECISION      HOWFAR
      DOUBLE PRECISION      DENOM
      DOUBLE PRECISION      SIZE1
      DOUBLE PRECISION      SIZE2

      LOGICAL               FAIL

      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )

      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD

      LOGICAL               VALNAN
      LOGICAL               EXPNAN




      CALL TSTSTY ( GOOD, BAD )
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ', &
                    'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )

!
!     Set placeholder message.
!
      MESSGE = 'This is a placeholder message that should have never '// &
               'been displayed. It indicates a CHCKAD bug.'


      FAIL = .FALSE.

!
!     Before doing any checks, verify that all array values are not
!     NaN's.
!
      DO I = 1, SIZE

!
!        Test both actual and expected array values to see whether they
!        are NaN.
!
         VALNAN =        ( .NOT. ( ARRAY(I) .GT. 1.D0 ) ) &
                   .AND. ( .NOT. ( ARRAY(I) .LT. 1.D0 ) ) &
                   .AND. ( .NOT. ( ARRAY(I) .EQ. 1.D0 ) )

         EXPNAN =        ( .NOT. ( EXP(I)   .GT. 1.D0 ) ) &
                   .AND. ( .NOT. ( EXP(I)   .LT. 1.D0 ) ) &
                   .AND. ( .NOT. ( EXP(I)   .EQ. 1.D0 ) )


         IF ( VALNAN .OR. EXPNAN ) THEN

!
!           At least one of the input values at this index is NaN; the
!           normal comparisons won't tell us anything. We need to
!           construct a custom error message.
!
            FAIL = .TRUE.

            IF ( .NOT. VALNAN ) THEN
!
!              Only EXP(I) is NaN.
!
               MESSGE = 'The value # of array # was #. ' // &
                        'The expected value was NaN.'
               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMD ( MESSGE, '#', ARRAY(I), 14,        MESSGE )

            ELSE IF ( .NOT. EXPNAN ) THEN
!
!              Only ARRAY(I) is NaN.
!
               MESSGE = 'The value # of array # was NaN. ' // &
                        'The expected value was #.'
               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMD ( MESSGE, '#', EXP(I), 14,          MESSGE )

            ELSE

               MESSGE = 'The value # of array # and the expected ' // &
                        'value were NaN.'
               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )

            END IF

            CALL TSTLOG( ' ',    FAIL )
            CALL TSTLOG( MESSGE, FAIL )
            CALL TSTLGS ( GOOD, BAD )
            OK = .NOT. FAIL
            RETURN

         END IF

      END DO

!
!     Do checks.
!
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
               CALL REPMD ( MESSGE, '#', ARRAY(I),  14,       MESSGE )
               CALL REPMD ( MESSGE, '#', EXP(I),    14,       MESSGE )
               CALL REPMD ( MESSGE, '#', ARRAY(I)-EXP(I), 14, MESSGE )
               CALL TSTLOG( ' ',    FAIL )
               CALL TSTLOG( MESSGE, FAIL )
               CALL TSTLGS ( GOOD, BAD )
               OK = .NOT. FAIL
               RETURN
            END IF
         END DO

      ELSE IF ( COMP .EQ. '~'  ) THEN

         DO I = 1, SIZE

            FAIL   = TOL .LT. DABS( ARRAY(I) - EXP(I) )

            IF ( FAIL ) THEN
               MESSGE = 'Value # of array # was not within # of #. ' &
               //       '/cr/cr The value was # ./cr' &
               //       'The difference ' &
               //       'between the actual and expected value ' &
               //       'was #. '
               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMD ( MESSGE, '#', TOL,      14,        MESSGE )
               CALL REPMD ( MESSGE, '#', EXP(I),   14,        MESSGE )
               CALL REPMD ( MESSGE, '#', ARRAY(I), 14,        MESSGE )
               CALL REPMD ( MESSGE, '#', ARRAY(I)-EXP(I), 14, MESSGE )
               CALL TSTLOG ( ' ',   FAIL )
               CALL TSTLOG( MESSGE, FAIL )
               CALL TSTLGS ( GOOD, BAD )
               OK = .NOT. FAIL
               RETURN
            END IF
         END DO

      ELSE IF ( COMP .EQ. '~/' ) THEN

         DO I = 1, SIZE

            IF ( ARRAY(I) .EQ. EXP(I) ) THEN
               REL = 0.0D0
            ELSE
               DENOM = MAX ( DABS(ARRAY(I)), DABS(EXP(I)) )
               REL   = DABS(      ARRAY(I) -      EXP(I)  ) /  DENOM
            END IF

            FAIL = TOL .LT. REL

            IF ( FAIL ) THEN

               MESSGE = 'Value # of array # was #. /cr ' &
               //       'The expected value was #. /cr/cr ' &
               //       'The relative difference between ' &
               //       'this component and its expected ' &
               //       'value was #. /cr/cr' &
               //       'The maximum relative difference allowed ' &
               //       'for a successful test is #. '
               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMD ( MESSGE, '#', ARRAY(I), 14,        MESSGE )
               CALL REPMD ( MESSGE, '#', EXP(I),   14,        MESSGE )
               CALL REPMD ( MESSGE, '#', REL, 14,             MESSGE )
               CALL REPMD ( MESSGE, '#', TOL, 14,             MESSGE )
               CALL TSTLOG ( ' ',   FAIL )
               CALL TSTLOG( MESSGE, FAIL )
               CALL TSTLGS ( GOOD, BAD )
               OK = .NOT. FAIL
               RETURN

            END IF

         END DO

      ELSE IF ( COMP .EQ. '||' ) THEN

         ANGLE = VSEPG ( ARRAY, EXP, SIZE )
         FAIL  = ANGLE .GT. TOL

         IF ( FAIL ) THEN
            MESSGE = 'The "angle" between direction of the #-vector ' &
         //          '# and the expected direction was #./cr/cr The ' &
         //          'maximum allowed angle for a successful test ' &
         //          'is #. '
            CALL REPMI ( MESSGE, '#',  SIZE,                  MESSGE )
            CALL REPMC ( MESSGE, '#',  NAME(1:RTRIM(NAME)),   MESSGE )
            CALL REPMD ( MESSGE, '#',  ANGLE, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#',  TOL,   14,             MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '~~' ) THEN

         HOWFAR = VDISTG ( ARRAY, EXP, SIZE )
         FAIL   = HOWFAR .GT. TOL

         IF ( FAIL ) THEN
            MESSGE = 'The "distance" between  the #-vector # and ' &
         //          'the expected vector was #./cr/cr  The maximum ' &
         //          'allowed distance for a successful test is #. '
            CALL REPMI ( MESSGE, '#',  SIZE,                  MESSGE )
            CALL REPMC ( MESSGE, '#',  NAME(1:RTRIM(NAME)),   MESSGE )
            CALL REPMD ( MESSGE, '#',  HOWFAR, 14,            MESSGE )
            CALL REPMD ( MESSGE, '#',  TOL,    14,            MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '~~/' ) THEN

         HOWFAR = VDISTG ( ARRAY, EXP, SIZE )
         SIZE1  = VNORMG ( ARRAY,      SIZE )
         SIZE2  = VNORMG ( EXP,        SIZE )

         IF ( (SIZE1 .EQ. 0.D0 ) .AND.  (SIZE2 .EQ. 0.D0) ) THEN
            REL = 0.0D0
         ELSE
            REL = HOWFAR / MAX (SIZE1,SIZE2)
         END IF

         FAIL   = REL .GT. TOL

         IF ( FAIL ) THEN
            MESSGE = 'The vector relative difference between the ' &
         //          '#-vector # and the expected vector was #./cr/cr ' &
         //          'The maximum allowed relative difference for a ' &
         //          'successful test is #. '

            CALL REPMI ( MESSGE, '#',  SIZE,                  MESSGE )
            CALL REPMC ( MESSGE, '#',  NAME(1:RTRIM(NAME)),   MESSGE )
            CALL REPMD ( MESSGE, '#',  REL   , 14,            MESSGE )
            CALL REPMD ( MESSGE, '#',  TOL,    14,            MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '|_' ) THEN

         DEFECT = DABS( HALFPI() - VSEPG( ARRAY, EXP, SIZE ) )
         FAIL   = DEFECT .GT. TOL

         IF ( FAIL ) THEN
            MESSGE = 'The "angle" between direction of the #-vector ' &
         //          '# and the expected direction was # radians away ' &
         //          'from being pi/2./cr/cr The ' &
         //          'maximum allowed defect in angle allowed for a ' &
         //          'successful test ' &
         //          'is #. '
            CALL REPMI ( MESSGE, '#',  SIZE,                  MESSGE )
            CALL REPMC ( MESSGE, '#',  NAME(1:RTRIM(NAME)),   MESSGE )
            CALL REPMD ( MESSGE, '#',  DEFECT, 14,            MESSGE )
            CALL REPMD ( MESSGE, '#',  TOL,    14,            MESSGE )
         END IF

      ELSE

         FAIL = .TRUE.

         MESSGE = 'The comparison "#" is not recognized. '
         CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )

      END IF

      IF ( .NOT. FAIL ) THEN

         IF ( VERBOS() ) THEN

            MESSGE = 'The comparison /cr/cr(3:)''# # EXPECTED'' ' &
         //          '/cr/cr(-3:) ' &
         //          'was satisfied to the ' &
         //          'specified tolerance: #.'

            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )
            CALL REPMD ( MESSGE, '#', TOL, 14,             MESSGE )

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
