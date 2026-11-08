C$Procedure CHCKAD ( Check an array of double precision values )
 
      SUBROUTINE CHCKAD ( NAME, ARRAY, COMP, EXP, SIZE,  TOL, OK )
 
C$ Abstract
C
C     Check the ordering of values in a d.p. array.
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
C      None.
C
C$ Keywords
C
C       TESTING
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         NAME
      DOUBLE PRECISION      ARRAY ( * )
      CHARACTER*(*)         COMP
      DOUBLE PRECISION      EXP   ( * )
      INTEGER               SIZE
      DOUBLE PRECISION      TOL
      LOGICAL               OK
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NAME       I   the name of the array to be examined.
C      ARRAY      I   the actual array
C      COMP       I   the kind of comparison to perform.
C      EXP        I   the comparison values for the array
C      SIZE       I   the size of the input array
C      TOL        I   the tolerance allowed in comparing.
C      OK         O   TRUE if the test passes, FALSE otherwise..
C
C$ Detailed_Input
C
C     NAME        is the string used to give the name of an array.
C
C     ARRAY       is the actual d.p. array to be examined
C
C     COMP        a string giving the kind of comparison to perform:
C
C                    =    ---   check for strict equality
C                    ~    ---   check for ARRAY(I) ~  EXP(I) for each I.
C                               This checks that the difference
C                               |ARRAY(I) - EXP(I)| <= TOL for all I.
C
C                    ~/   ---   check for ARRAY(I) ~/ EXP(I) ( Relative
C                               difference between ARRAY(I) and EXP(I)
C                               .LE. TOL for all I.
C
C                    ||   ---   check that the "angle" between ARRAY
C                               and EXP is within TOL of zero.
C
C                    ~~   ---   check that the L2 distance between
C                               ARRAY and EXP is within TOL of zero.
C
C                    |_   ---   check that the angle between ARRAY
C                               and EXP is within TOL of pi/2 radians
C
C                    ~~/  ---   check that the vector relative
C                               difference between ARRAY and EXP is
C                               within TOL of zero.
C
C     EXP         an expected values or bounds on the values in ARRAY.
C
C
C     TOL        is a "tolerance" to use when checking for VAL to
C                be nearly the same as EXP.
C
C$ Detailed_Output
C
C     OK         if the check of the input array is successful then
C                OK is given the value TRUE.  Otherwise OK is given the
C                value FALSE and a diagnostic message is sent to the
C                test logger.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This routine handles a wide variety of comparisons between
C     double precision arrays.
C
C$ Examples
C
C     Suppose that you have just made a call to a subroutine that
C     you wish to test (call the routine SPUD) and you would like
C     to test an output d.p. against an expected value and verify that
C     the relative difference is less than some value.  Using
C     this routine you can automatically have the test result logged
C     in via the testing utilities.
C
C        CALL SPUD   (  INPUT,   OUTPUT )
C        CALL CHCKSD ( 'OUTPUT', OUTPUT, '~/', EXPECT, 1.0D-12, OK )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     E.W. Wright     (JPL)
C     B.V. Semenov    (JPL)
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    Testing Utility Version 1.3.0, 23-APR-2014 (BVS)(NJB)
C
C        Added checks for expected or actual values equal to NaN.
C
C        Tweaked logic to do only necessary output message construction
C        operations to speed-up execution.
C
C-    Testing Utility Version 1.2.0, 20-OCT-2006 (EDW)
C
C        Corrected logic error in the ~~/ test block. Replaced the 
C        line
C
C           IF ( SIZE1 .EQ. SIZE2 ) THEN
C
C        with
C
C           IF ( (SIZE1 .EQ. 0.D0 ) .AND. (SIZE2 .EQ. 0.D0) ) THEN
C
C-    Testing Utility Version 1.1.0, 10-MAY-2001 (FST)
C
C        OK is now properly set in all cases before returning
C        control to the caller.
C
C-    Testing Utility Version 1.0.0, 7-NOV-1994 (WLT)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VSEPG
      DOUBLE PRECISION      VDISTG
      DOUBLE PRECISION      VNORMG
      DOUBLE PRECISION      HALFPI
 
      INTEGER               RTRIM
C
C     Testing Utility Functions
C
      LOGICAL               VERBOS
C
C     Local Variables
C
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
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ',
     .              'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )
 
C
C     Set placeholder message.
C 
      MESSGE = 'This is a placeholder message that should have never '//
     .         'been displayed. It indicates a CHCKAD bug.' 
 
 
      FAIL = .FALSE.

C
C     Before doing any checks, verify that all array values are not
C     NaN's.
C
      DO I = 1, SIZE

C
C        Test both actual and expected array values to see whether they
C        are NaN.
C 
         VALNAN =        ( .NOT. ( ARRAY(I) .GT. 1.D0 ) )
     .             .AND. ( .NOT. ( ARRAY(I) .LT. 1.D0 ) )
     .             .AND. ( .NOT. ( ARRAY(I) .EQ. 1.D0 ) )
         
         EXPNAN =        ( .NOT. ( EXP(I)   .GT. 1.D0 ) )
     .             .AND. ( .NOT. ( EXP(I)   .LT. 1.D0 ) )
     .             .AND. ( .NOT. ( EXP(I)   .EQ. 1.D0 ) )


         IF ( VALNAN .OR. EXPNAN ) THEN

C
C           At least one of the input values at this index is NaN; the
C           normal comparisons won't tell us anything. We need to
C           construct a custom error message.
C
            FAIL = .TRUE.

            IF ( .NOT. VALNAN ) THEN
C
C              Only EXP(I) is NaN.
C
               MESSGE = 'The value # of array # was #. ' //
     .                  'The expected value was NaN.'
               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMD ( MESSGE, '#', ARRAY(I), 14,        MESSGE )

            ELSE IF ( .NOT. EXPNAN ) THEN
C
C              Only ARRAY(I) is NaN.
C
               MESSGE = 'The value # of array # was NaN. ' //
     .                  'The expected value was #.'
               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMD ( MESSGE, '#', EXP(I), 14,          MESSGE )

            ELSE

               MESSGE = 'The value # of array # and the expected ' //
     .                  'value were NaN.'
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

C
C     Do checks.
C 
      IF      ( COMP .EQ. '='  ) THEN
 
         DO I = 1, SIZE
 
            FAIL = ARRAY(I) .NE. EXP(I)
 
            IF ( FAIL ) THEN
               MESSGE = 'Value # of array # was not the value '
     .         //       'expected. /cr(3:)/cr '
     .         //       'The value was:         # /cr'
     .         //       'the expected value was #./cr'
     .         //       'The difference between these is: # . '
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
               MESSGE = 'Value # of array # was not within # of #. '
     .         //       '/cr/cr The value was # ./cr'
     .         //       'The difference '
     .         //       'between the actual and expected value '
     .         //       'was #. '
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
 
               MESSGE = 'Value # of array # was #. /cr '
     .         //       'The expected value was #. /cr/cr '
     .         //       'The relative difference between '
     .         //       'this component and its expected '
     .         //       'value was #. /cr/cr'
     .         //       'The maximum relative difference allowed '
     .         //       'for a successful test is #. '
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
            MESSGE = 'The "angle" between direction of the #-vector '
     .   //          '# and the expected direction was #./cr/cr The '
     .   //          'maximum allowed angle for a successful test '
     .   //          'is #. '
            CALL REPMI ( MESSGE, '#',  SIZE,                  MESSGE )
            CALL REPMC ( MESSGE, '#',  NAME(1:RTRIM(NAME)),   MESSGE )
            CALL REPMD ( MESSGE, '#',  ANGLE, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#',  TOL,   14,             MESSGE )
         END IF 
 
      ELSE IF ( COMP .EQ. '~~' ) THEN
 
         HOWFAR = VDISTG ( ARRAY, EXP, SIZE )
         FAIL   = HOWFAR .GT. TOL
 
         IF ( FAIL ) THEN
            MESSGE = 'The "distance" between  the #-vector # and '
     .   //          'the expected vector was #./cr/cr  The maximum '
     .   //          'allowed distance for a successful test is #. '
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
            MESSGE = 'The vector relative difference between the '
     .   //          '#-vector # and the expected vector was #./cr/cr '
     .   //          'The maximum allowed relative difference for a '
     .   //          'successful test is #. '
 
            CALL REPMI ( MESSGE, '#',  SIZE,                  MESSGE )
            CALL REPMC ( MESSGE, '#',  NAME(1:RTRIM(NAME)),   MESSGE )
            CALL REPMD ( MESSGE, '#',  REL   , 14,            MESSGE )
            CALL REPMD ( MESSGE, '#',  TOL,    14,            MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '|_' ) THEN
 
         DEFECT = DABS( HALFPI() - VSEPG( ARRAY, EXP, SIZE ) )
         FAIL   = DEFECT .GT. TOL
 
         IF ( FAIL ) THEN
            MESSGE = 'The "angle" between direction of the #-vector '
     .   //          '# and the expected direction was # radians away '
     .   //          'from being pi/2./cr/cr The '
     .   //          'maximum allowed defect in angle allowed for a '
     .   //          'successful test '
     .   //          'is #. '
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
 
            MESSGE = 'The comparison /cr/cr(3:)''# # EXPECTED'' '
     .   //          '/cr/cr(-3:) '
     .   //          'was satisfied to the '
     .   //          'specified tolerance: #.'
 
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
