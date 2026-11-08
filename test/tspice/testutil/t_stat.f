C$Procedure      T_STAT ( Test Status )

      SUBROUTINE T_STAT ( ACT, NAME, OK, NUMBER )

C$ Abstract
C
C     This routine provides an umbrella for a number of entry points
C     used for setting and retrieving the status of a family of
C     related test cases.
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
C     TESTING
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         ACT
      CHARACTER*(*)         NAME
      LOGICAL               OK
      INTEGER               NUMBER

C$ Brief_I/O
C
C     VARIABLE  I/O  ENTRY POINT
C     --------  ---  --------------------------------------------------
C     ACT        I   T_TRACE
C     NAME      I/O  T_BEGIN, T_NAME, T_CASE, T_CNAME, T_TRACE
C     OK         O   T_SUCCESS, T_CPASS, T_ANYBAD
C     NUMBER     O   T_FCOUNT, T_CNUM
C
C$ Detailed_Input
C
C     See individual entry points for descriptions
C
C$ Detailed_Output
C
C     See individual entry points for descriptions
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     This routine has no calls to any external routines.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine maintains the status of a family of related test
C     routines (provided of course that users) make proper use
C     of the various entry points.  The entry points are:
C
C     T_BEGIN ( NAME )  --- set the NAME of the current family of
C                           tests. The family can have a name of up to
C                           MAXFNM characters.
C
C     T_NAME  ( NAME )  --- return the name of the current family of
C                           tests
C
C     T_SUCCESS ( OK )  --- have all tests in this family passed to
C                           this point.
C
C     T_CASE  ( NAME )  --- gives a name (possibly blank) to the
C                           current test case. Set the current case
C                           success to .TRUE. The name may be up to
C                           MAXCNM characters.
C
C     T_CFAIL ( )       --- set the success of the current case to
C                          .FALSE.
C
C     T_CPASS ( OK )    --- return the success of the current test
C                           case.
C
C     T_CNUM  (NUMBER)  --- return the current test case number.
C
C     T_CNAME ( NAME )  --- return the full name of the current test
C                           case.  The full name returned may have up
C                           to MAXCNM characters.
C
C     T_FCOUNT(NUMBER)  --- the number of test cases that failed.
C
C     T_TRACE ( ACT, NAME ) --- set or get the last stored value of
C                               the SPICELIB traceback.
C
C
C     Note that most of the routines are meant to be used in building
C     other somewhat higher test utilities.
C
C$ Examples
C
C     A sample test case program would be outlined as shown here:
C
C     First establish the test name.
C
C     CALL T_BEGIN ( 'MY_ROUTINE' )
C
C
C        perform any test initializations.
C
C
C     CALL T_CASE ( 'Normal Inputs' )
C
C     set up the inputs and expected outputs.
C
C     IF ( not success ) THEN
C        CALL T_CFAIL ()
C     END IF
C
C     All the bookkeeping about the success or failure
C     is maintained in one place we can deal with a failure
C     (even though we may have several tests above) here.
C
C     CALL T_CPASS ( OK )
C
C     IF ( .NOT. OK ) THEN
C        CALL T_CNAME ( NAME )
C
C        WRITE (*,*) NAME
C        WRITE (*,*) 'Did not pass. '
C
C     END IF
C
C
C     DO I = 1, NEXCEP
C
C        CALL T_CASE ( 'Exception' )
C
C
C        set up this case
C
C        IF ( not success ) THEN
C           CALL T_CFAIL()
C        END IF
C
C
C        CALL T_CPASS ( OK )
C
C        IF ( .NOT. OK ) THEN
C           CALL T_CNAME ( NAME )
C
C           WRITE (*,*) NAME
C           WRITE (*,*) 'Did not pass. '
C
C        END IF
C
C     END DO
C
C
C     CALL T_SUCCESS ( OK   )
C     CALL T_NAME    ( NAME )
C
C     IF ( .NOT. OK ) THEN
C
C        WRITE (*,*) 'The ', NAME, ' family of tests failed. '
C        WRITE (*,*)
C     ELSE
C        WRITE (*,*) 'All tests of ', NAME, ' passed.'
C     END IF
C
C
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 3.0.1, 06-DEC-2021 (JDR)
C
C        Added description of ACT input argument. Added NAIF standard
C        compliant headers to all entry points.
C
C-    Testing Utilities Version 3.0.0, 29-MAY-2010 (NJB)
C
C        Updated parameters to increase max length of test family names
C        from 12 characters to MAXFNM and increase max length of test
C        case names from 80 to MAXCNM.
C
C-    Testing Utilities Version 2.1.0, 18-JUN-1999 (WLT)
C
C        Added RETURN before first entry point.
C
C-    Testing Utilities Version 2.0.0, 20-JUN-1997 (WLT)
C
C        Added the entry point that allows users to set or get
C        the current traceback.
C
C-    TESTLIB Version 1.0.0, 25-OCT-1994 (WLT)
C
C
C-&

C$ Index_Entries
C
C     Keep track of testing status.
C
C-&


C
C     Local parameters
C
      INTEGER               MAXFNM
      PARAMETER           ( MAXFNM = 80 )

      INTEGER               MAXCNM
      PARAMETER           ( MAXCNM = 1000 )

      INTEGER               LNGSTR
      PARAMETER           ( LNGSTR = 1200 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

C
C     Local Variables
C
      LOGICAL               SUCCES
      LOGICAL               CASEOK
      INTEGER               CASENO
      INTEGER               NFAIL


      CHARACTER*(MAXCNM)    DESCR
      CHARACTER*(MAXCNM)    MYNAME
      CHARACTER*(WDSIZE)    MYACT
      CHARACTER*(LNGSTR)    TRACE
      CHARACTER*(MAXFNM)    RNAME

C
C     We enough digits to have unique text versions of case numbers
C     for 10**7 cases.
C
      INTEGER               MAXPLC
      PARAMETER           ( MAXPLC = 7 )

      CHARACTER*(1)         DIGIT( 10, MAXPLC )
      INTEGER               D    (     MAXPLC )
      INTEGER               I
      INTEGER               J
      LOGICAL               INCR
      LOGICAL               ANYBAD

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                  ANYBAD   / .FALSE. /
      DATA                  SUCCES   / .TRUE.  /
      DATA                  CASEOK   / .TRUE.  /
      DATA                  CASENO   /  0      /
      DATA                  NFAIL    /  0      /
      DATA                  MYNAME   / ' '     /
      DATA                  DESCR    / ' '     /
      DATA                  RNAME    / ' '     /

      RETURN



C$Procedure T_BEGIN ( Initialize test case family )

      ENTRY T_BEGIN ( NAME )

C$ Abstract
C
C     Sets up all of the needed local
C     machinery for tracking the test cases for the current
C     set of tests.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of a test family or test case.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Examples
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Initialize test case family.
C
C-&


C
C        Until something goes wrong.  All tests are assumed to pass.
C        Store the name and current case number.
C
         SUCCES = .TRUE.
         RNAME  =  NAME
         CASENO =  0
         NFAIL  =  0
C
C        Set up the "odometer" for the case counter.  Note we put
C        blanks instead of zeros for every digit but the least
C        significant digit.
C
C        Here's the idea.  At each test case, we increment CASENO
C        and flip over one digit on the case odometer.  When we
C        reach the 10 on any digit, we flip back to zero and
C        cause the digit to the left to increment.  The actual
C        text version for the current case number will be:
C
C        DIGIT(D(1),1)//DIGIT(D(2),2)// ... //DIGIT(D(MAXPLC),MAXPLC).
C

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



C$Procedure T_NAME ( Return the name of the current family of tests )

      ENTRY T_NAME    ( NAME   )

C$ Abstract
C
C     Returns the name of the current family of tests.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       O   Name of the current family of tests.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Examples
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Name of the current family of tests.
C
C-&

         NAME = RNAME
         RETURN



C$Procedure T_SUCCESS ( Return information on overall status of tests )

      ENTRY T_SUCCESS ( OK )

C$ Abstract
C
C     Return information about whether all test cases since set up
C     via T_BEGIN have passed.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     LOGICAL               OK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   The success of the family of cases.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Examples
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Information on overall status of tests.
C
C-&

         OK = SUCCES
         RETURN


C$Procedure T_FCOUNT ( Number of failed test cases )

      ENTRY T_FCOUNT  ( NUMBER )

C$ Abstract
C
C     Returns the number of failed test cases.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     INTEGER               NUMBER
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NUMBER     O   Number of failed test cases.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Examples
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Number of failed tests
C
C-&
         NUMBER = NFAIL
         RETURN



C$Procedure T_CASE ( Set up for the next test case )

      ENTRY T_CASE ( NAME )

C$ Abstract
C
C     Set up for the next test case.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the next test case.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Examples
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Set up for the next test case.
C
C-&


C
C        Store the name, set this case success value to .TRUE. and
C        increment the case number.
C
         DESCR  =  NAME
         CASEOK = .TRUE.
         CASENO =  CASENO + 1
C
C        Reset the error handling.
C
         CALL RESET ()

C
C        Next we increment the odometer.  We always increment the least
C        significant digit.
C
         INCR   = .TRUE.

         DO I = MAXPLC, 1, -1

            IF ( INCR ) THEN
C
C              If we incremented this digit, we will never want to use
C              a blank (while in this test case) for this digit again.
C              Set the first digit to zero "0".
C
               D(I)       =  D(I) + 1
               DIGIT(1,I) = '0'
C
C              If the current digit is not greater than 10, we need
C              to set it back to 1 and cascade up to the next digit.
C              That is, make sure INCR stays .TRUE.
C
               IF ( D(I) .GT. 10 ) THEN

                  D(I) =  1

               ELSE
C
C                 In this case we do not need to increment any digit
C                 that is more significant than the current digit.
C
                  INCR = .FALSE.

               END IF

            END IF

         END DO
         RETURN



C$Procedure T_CFAIL ( Records some aspects of failed test case )

      ENTRY T_CFAIL ( )

C$ Abstract
C
C     This entry point merely records that some aspect of this
C     test case has failed.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     None.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Examples
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Recording of some aspects of failed test case
C
C-&


         IF ( CASEOK ) THEN
            NFAIL = NFAIL + 1
         END IF

         ANYBAD = .TRUE.
         CASEOK = .FALSE.
         SUCCES = .FALSE.
         RETURN




C$Procedure T_CPASS ( Return status of current test case )

      ENTRY T_CPASS   ( OK )

C$ Abstract
C
C     Return information about whether or not the current test case
C     passed.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     LOGICAL               OK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   The success of the current test case.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Examples
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Status of the current test case.
C
C-&

         OK = CASEOK
         RETURN



C$Procedure T_CNUM ( Number of current test case )

      ENTRY T_CNUM    ( NUMBER )

C$ Abstract
C
C     Return the number of the current test case.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     INTEGER               NUMBER
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NUMBER     O   Number of current test case.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Examples
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Number of current test case.
C
C-&
         NUMBER = CASENO
         RETURN



C$Procedure T_CNAME ( Return the name of the current test case )

      ENTRY T_CNAME ( NAME )

C$ Abstract
C
C     The entry point T_CNAME gets the name of the current test case.
C     This has the form "Test Case #. <description> " where # is
C     the text decimal for the current case and <description> is the
C     description provided at the entry T_CASE.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       O   Name of the current test case.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Examples
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Name of the current test case.
C
C-&
         MYNAME = 'Test Case '
         J      =  10
C
C        Append each non-blank digit (starting with the most
C        significant) to the current value of MYNAME.
C
         DO I = 1, MAXPLC
            IF ( DIGIT(D(I),I) .NE. ' ' ) THEN
               J         = J + 1
               MYNAME(J:J) = DIGIT(D(I),I)
            END IF
         END DO
C
C        Place a period after the case number.
C
         J           = J + 1
         MYNAME(J:J) = '.'
C
C        Follow that up with the case description.
C
         J          = J + 2
         MYNAME(J:) = DESCR
         NAME       = MYNAME
         RETURN


C$Procedure T_ANYBAD ( Was there any failed case? )

      ENTRY T_ANYBAD ( OK )

C$ Abstract
C
C     Returns information about whether any test cases have failed
C     in the overall test campaign (over multiple test families).
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     LOGICAL               OK
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   The success of the family of cases.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Examples
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 06-DEC-2021 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Information on where any test failed in test campaign
C
C-&

         OK = ANYBAD
         RETURN


C$Procedure T_TRACE ( Set or get the last stored value of traceback )

      ENTRY T_TRACE ( ACT, NAME )

C$ Abstract
C
C     Set or get the last stored value of the SPICELIB traceback.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         ACT
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ACT        I   Action, either 'GET' or 'SET'.
C     NAME      I-O  SPICELIB traceback.
C
C$ Detailed_Input
C
C     None.
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
C     None.
C
C$ Examples
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 25-OCT-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Set the last stored value of the SPICELIB traceback.
C     Get the last stored value of the SPICELIB traceback.
C
C-&

         CALL LJUST ( ACT,   MYACT )
         CALL UCASE ( MYACT, MYACT )

         IF ( MYACT .EQ. 'SET' ) THEN
            TRACE = NAME
         ELSE IF ( MYACT .EQ. 'GET' ) THEN
            NAME = TRACE
         END IF

         RETURN

      END
