!$Procedure      T_STAT ( Test Status )

      SUBROUTINE T_STAT ( ACT, NAME, OK, NUMBER )
      use pepper_legacy, pepper_legacy_trace => trace

! $ Abstract
!
!     This routine provides an umbrella for a number of entry points
!     used for setting and retrieving the status of a family of
!     related test cases.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         ACT
      CHARACTER*(*)         NAME
      LOGICAL               OK
      INTEGER               NUMBER

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINT
!     --------  ---  --------------------------------------------------
!     ACT        I   T_TRACE
!     NAME      I/O  T_BEGIN, T_NAME, T_CASE, T_CNAME, T_TRACE
!     OK         O   T_SUCCESS, T_CPASS, T_ANYBAD
!     NUMBER     O   T_FCOUNT, T_CNUM
!
! $ Detailed_Input
!
!     See individual entry points for descriptions
!
! $ Detailed_Output
!
!     See individual entry points for descriptions
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     This routine has no calls to any external routines.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine maintains the status of a family of related test
!     routines (provided of course that users) make proper use
!     of the various entry points.  The entry points are:
!
!     T_BEGIN ( NAME )  --- set the NAME of the current family of
!                           tests. The family can have a name of up to
!                           MAXFNM characters.
!
!     T_NAME  ( NAME )  --- return the name of the current family of
!                           tests
!
!     T_SUCCESS ( OK )  --- have all tests in this family passed to
!                           this point.
!
!     T_CASE  ( NAME )  --- gives a name (possibly blank) to the
!                           current test case. Set the current case
!                           success to .TRUE. The name may be up to
!                           MAXCNM characters.
!
!     T_CFAIL ( )       --- set the success of the current case to
!                          .FALSE.
!
!     T_CPASS ( OK )    --- return the success of the current test
!                           case.
!
!     T_CNUM  (NUMBER)  --- return the current test case number.
!
!     T_CNAME ( NAME )  --- return the full name of the current test
!                           case.  The full name returned may have up
!                           to MAXCNM characters.
!
!     T_FCOUNT(NUMBER)  --- the number of test cases that failed.
!
!     T_TRACE ( ACT, NAME ) --- set or get the last stored value of
!                               the SPICELIB traceback.
!
!
!     Note that most of the routines are meant to be used in building
!     other somewhat higher test utilities.
!
! $ Examples
!
!     A sample test case program would be outlined as shown here:
!
!     First establish the test name.
!
!     CALL T_BEGIN ( 'MY_ROUTINE' )
!
!
!        perform any test initializations.
!
!
!     CALL T_CASE ( 'Normal Inputs' )
!
!     set up the inputs and expected outputs.
!
!     IF ( not success ) THEN
!        CALL T_CFAIL ()
!     END IF
!
!     All the bookkeeping about the success or failure
!     is maintained in one place we can deal with a failure
!     (even though we may have several tests above) here.
!
!     CALL T_CPASS ( OK )
!
!     IF ( .NOT. OK ) THEN
!        CALL T_CNAME ( NAME )
!
!        WRITE (*,*) NAME
!        WRITE (*,*) 'Did not pass. '
!
!     END IF
!
!
!     DO I = 1, NEXCEP
!
!        CALL T_CASE ( 'Exception' )
!
!
!        set up this case
!
!        IF ( not success ) THEN
!           CALL T_CFAIL()
!        END IF
!
!
!        CALL T_CPASS ( OK )
!
!        IF ( .NOT. OK ) THEN
!           CALL T_CNAME ( NAME )
!
!           WRITE (*,*) NAME
!           WRITE (*,*) 'Did not pass. '
!
!        END IF
!
!     END DO
!
!
!     CALL T_SUCCESS ( OK   )
!     CALL T_NAME    ( NAME )
!
!     IF ( .NOT. OK ) THEN
!
!        WRITE (*,*) 'The ', NAME, ' family of tests failed. '
!        WRITE (*,*)
!     ELSE
!        WRITE (*,*) 'All tests of ', NAME, ' passed.'
!     END IF
!
!
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 3.0.1, 06-DEC-2021 (JDR)
!
!        Added description of ACT input argument. Added NAIF standard
!        compliant headers to all entry points.
!
!-    Testing Utilities Version 3.0.0, 29-MAY-2010 (NJB)
!
!        Updated parameters to increase max length of test family names
!        from 12 characters to MAXFNM and increase max length of test
!        case names from 80 to MAXCNM.
!
!-    Testing Utilities Version 2.1.0, 18-JUN-1999 (WLT)
!
!        Added RETURN before first entry point.
!
!-    Testing Utilities Version 2.0.0, 20-JUN-1997 (WLT)
!
!        Added the entry point that allows users to set or get
!        the current traceback.
!
!-    TESTLIB Version 1.0.0, 25-OCT-1994 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Keep track of testing status.
!
!-&


!
!     Local parameters
!
      INTEGER               MAXFNM
      PARAMETER           ( MAXFNM = 80 )

      INTEGER               MAXCNM
      PARAMETER           ( MAXCNM = 1000 )

      INTEGER               LNGSTR
      PARAMETER           ( LNGSTR = 1200 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

!
!     Local Variables
!
      LOGICAL               SUCCES
      LOGICAL               CASEOK
      INTEGER               CASENO
      INTEGER               NFAIL


      CHARACTER*(MAXCNM)    DESCR
      CHARACTER*(MAXCNM)    MYNAME
      CHARACTER*(WDSIZE)    MYACT
      CHARACTER*(LNGSTR)    TRACE
      CHARACTER*(MAXFNM)    RNAME

!
!     We enough digits to have unique text versions of case numbers
!     for 10**7 cases.
!
      INTEGER               MAXPLC
      PARAMETER           ( MAXPLC = 7 )

      CHARACTER*(1)         DIGIT( 10, MAXPLC )
      INTEGER               D    (     MAXPLC )
      INTEGER               I
      INTEGER               J
      LOGICAL               INCR
      LOGICAL               ANYBAD

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  ANYBAD   / .FALSE. /
      DATA                  SUCCES   / .TRUE.  /
      DATA                  CASEOK   / .TRUE.  /
      DATA                  CASENO   /  0      /
      DATA                  NFAIL    /  0      /
      DATA                  MYNAME   / ' '     /
      DATA                  DESCR    / ' '     /
      DATA                  RNAME    / ' '     /

      RETURN



!$Procedure T_BEGIN ( Initialize test case family )

      ENTRY T_BEGIN ( NAME )

! $ Abstract
!
!     Sets up all of the needed local
!     machinery for tracking the test cases for the current
!     set of tests.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of a test family or test case.
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
!     Error free.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Initialize test case family.
!
!-&


!
!        Until something goes wrong.  All tests are assumed to pass.
!        Store the name and current case number.
!
         SUCCES = .TRUE.
         RNAME  =  NAME
         CASENO =  0
         NFAIL  =  0
!
!        Set up the "odometer" for the case counter.  Note we put
!        blanks instead of zeros for every digit but the least
!        significant digit.
!
!        Here's the idea.  At each test case, we increment CASENO
!        and flip over one digit on the case odometer.  When we
!        reach the 10 on any digit, we flip back to zero and
!        cause the digit to the left to increment.  The actual
!        text version for the current case number will be:
!
!        DIGIT(D(1),1)//DIGIT(D(2),2)// ... //DIGIT(D(MAXPLC),MAXPLC).
!

         DO I = 1, MAXPLC
            DIGIT (  1, I) = ' '
            DIGIT (  2, I) = '1'
            DIGIT (  3, I) = '2'
            DIGIT (  4, I) = '3'
            DIGIT (  5, I) = '4'
            DIGIT (  6, I) = '5'
            DIGIT (  7, I) = '6'
            DIGIT (  8, I) = '7'
            DIGIT (  9, I) = '8'
            DIGIT ( 10, I) = '9'
            D(I)           =  1
         END DO

         DIGIT( 1, MAXPLC ) = '0'


         RETURN



!$Procedure T_NAME ( Return the name of the current family of tests )

      ENTRY T_NAME    ( NAME   )

! $ Abstract
!
!     Returns the name of the current family of tests.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       O   Name of the current family of tests.
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
!     Error free.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Name of the current family of tests.
!
!-&

         NAME = RNAME
         RETURN



!$Procedure T_SUCCESS ( Return information on overall status of tests )

      ENTRY T_SUCCESS ( OK )

! $ Abstract
!
!     Return information about whether all test cases since set up
!     via T_BEGIN have passed.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     LOGICAL               OK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   The success of the family of cases.
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
!     Error free.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Information on overall status of tests.
!
!-&

         OK = SUCCES
         RETURN


!$Procedure T_FCOUNT ( Number of failed test cases )

      ENTRY T_FCOUNT  ( NUMBER )

! $ Abstract
!
!     Returns the number of failed test cases.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     INTEGER               NUMBER
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NUMBER     O   Number of failed test cases.
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
!     Error free.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Number of failed tests
!
!-&
         NUMBER = NFAIL
         RETURN



!$Procedure T_CASE ( Set up for the next test case )

      ENTRY T_CASE ( NAME )

! $ Abstract
!
!     Set up for the next test case.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the next test case.
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
!     Error free.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Set up for the next test case.
!
!-&


!
!        Store the name, set this case success value to .TRUE. and
!        increment the case number.
!
         DESCR  =  NAME
         CASEOK = .TRUE.
         CASENO =  CASENO + 1
!
!        Reset the error handling.
!
         CALL RESET ()

!
!        Next we increment the odometer.  We always increment the least
!        significant digit.
!
         INCR   = .TRUE.

         DO I = MAXPLC, 1, -1

            IF ( INCR ) THEN
!
!              If we incremented this digit, we will never want to use
!              a blank (while in this test case) for this digit again.
!              Set the first digit to zero "0".
!
               D(I)       =  D(I) + 1
               DIGIT(1,I) = '0'
!
!              If the current digit is not greater than 10, we need
!              to set it back to 1 and cascade up to the next digit.
!              That is, make sure INCR stays .TRUE.
!
               IF ( D(I) .GT. 10 ) THEN

                  D(I) =  1

               ELSE
!
!                 In this case we do not need to increment any digit
!                 that is more significant than the current digit.
!
                  INCR = .FALSE.

               END IF

            END IF

         END DO
         RETURN



!$Procedure T_CFAIL ( Records some aspects of failed test case )

      ENTRY T_CFAIL ( )

! $ Abstract
!
!     This entry point merely records that some aspect of this
!     test case has failed.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     None.
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
!     Error free.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Recording of some aspects of failed test case
!
!-&


         IF ( CASEOK ) THEN
            NFAIL = NFAIL + 1
         END IF

         ANYBAD = .TRUE.
         CASEOK = .FALSE.
         SUCCES = .FALSE.
         RETURN




!$Procedure T_CPASS ( Return status of current test case )

      ENTRY T_CPASS   ( OK )

! $ Abstract
!
!     Return information about whether or not the current test case
!     passed.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     LOGICAL               OK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   The success of the current test case.
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
!     Error free.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Status of the current test case.
!
!-&

         OK = CASEOK
         RETURN



!$Procedure T_CNUM ( Number of current test case )

      ENTRY T_CNUM    ( NUMBER )

! $ Abstract
!
!     Return the number of the current test case.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     INTEGER               NUMBER
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NUMBER     O   Number of current test case.
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
!     Error free.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Number of current test case.
!
!-&
         NUMBER = CASENO
         RETURN



!$Procedure T_CNAME ( Return the name of the current test case )

      ENTRY T_CNAME ( NAME )

! $ Abstract
!
!     The entry point T_CNAME gets the name of the current test case.
!     This has the form "Test Case #. <description> " where # is
!     the text decimal for the current case and <description> is the
!     description provided at the entry T_CASE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       O   Name of the current test case.
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
!     Error free.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Name of the current test case.
!
!-&
         MYNAME = 'Test Case '
         J      =  10
!
!        Append each non-blank digit (starting with the most
!        significant) to the current value of MYNAME.
!
         DO I = 1, MAXPLC
            IF ( DIGIT(D(I),I) .NE. ' ' ) THEN
               J         = J + 1
               MYNAME(J:J) = DIGIT(D(I),I)
            END IF
         END DO
!
!        Place a period after the case number.
!
         J           = J + 1
         MYNAME(J:J) = '.'
!
!        Follow that up with the case description.
!
         J          = J + 2
         MYNAME(J:) = DESCR
         NAME       = MYNAME
         RETURN


!$Procedure T_ANYBAD ( Was there any failed case? )

      ENTRY T_ANYBAD ( OK )

! $ Abstract
!
!     Returns information about whether any test cases have failed
!     in the overall test campaign (over multiple test families).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     LOGICAL               OK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   The success of the family of cases.
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
!     Error free.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 06-DEC-2021 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Information on where any test failed in test campaign
!
!-&

         OK = ANYBAD
         RETURN


!$Procedure T_TRACE ( Set or get the last stored value of traceback )

      ENTRY T_TRACE ( ACT, NAME )

! $ Abstract
!
!     Set or get the last stored value of the SPICELIB traceback.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         ACT
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ACT        I   Action, either 'GET' or 'SET'.
!     NAME      I-O  SPICELIB traceback.
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
!     Error free.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Set the last stored value of the SPICELIB traceback.
!     Get the last stored value of the SPICELIB traceback.
!
!-&

         CALL LJUST ( ACT,   MYACT )
         CALL UCASE ( MYACT, MYACT )

         IF ( MYACT .EQ. 'SET' ) THEN
            TRACE = NAME
         ELSE IF ( MYACT .EQ. 'GET' ) THEN
            NAME = TRACE
         END IF

         RETURN

      END
