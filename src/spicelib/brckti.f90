!$Procedure BRCKTI ( Bracket an integer value within an interval )

      integer  function brckti ( number, end1, end2 )

! $ Abstract
!
!     Bracket an integer number. That is, given a number and an
!     acceptable interval, make sure that the number is contained in the
!     interval. (If the number is already in the interval, leave it
!     alone. If not, set it to the nearest endpoint of the interval.)
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     INTERVALS
!     NUMBERS
!     UTILITY
!
! $ Declarations

      implicit none

      integer   number
      integer   end1
      integer   end2

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NUMBER     I   Number to be bracketed.
!     END1       I   One of the bracketing endpoints for NUMBER.
!     END2       I   The other bracketing endpoint for NUMBER.
!
!     The function returns the bracketed number.
!
! $ Detailed_Input
!
!     NUMBER   is the number to be bracketed. That is, the
!              value of NUMBER is constrained to lie in the
!              interval bounded by END1 and END2.
!
!     END1,
!     END2     are the lower and upper bounds for NUMBER. The
!              order is not important.
!
! $ Detailed_Output
!
!     The function returns the bracketed number. That is NUMBER, if it
!     was already in the interval provided. Otherwise the returned
!     value is the nearest bound of the interval.
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
!     This routine provides a shorthand notation for code fragments
!     like the following:
!
!        IF ( END1 .LT. END2 ) THEN
!           IF      ( NUMBER .LT. END1 ) THEN
!              NUMBER = END1
!           ELSE IF ( NUMBER .GT. END2 ) THEN
!              NUMBER = END2
!           END IF
!        ELSE
!           IF      ( NUMBER .LT. END2 ) THEN
!              NUMBER = END2
!           ELSE IF ( NUMBER .GT. END1 ) THEN
!              NUMBER = END1
!           END IF
!        END IF
!
!     which occur frequently during the processing of program inputs.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following code example illustrates the operation of
!        BRCKTI.
!
!        Example code begins here.
!
!
!              PROGRAM BRCKTI_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER                 BRCKTI
!
!        C
!        C     Local parameters.
!        C
!              INTEGER                 LISTSZ
!              PARAMETER             ( LISTSZ = 4  )
!
!        C
!        C     Local variables.
!        C
!              INTEGER                 END1    ( LISTSZ )
!              INTEGER                 END2    ( LISTSZ )
!              INTEGER                 I
!              INTEGER                 NUMBER  ( LISTSZ )
!
!        C
!        C     Set the values for the example.
!        C
!              DATA                    END1   /  1,  1,  10, -10 /
!              DATA                    END2   / 10, 10, -10,  -1 /
!              DATA                    NUMBER / -1, 29,   3,   3 /
!
!
!              WRITE(*,'(A)') 'Number  End1  End2  Bracketed'
!              WRITE(*,'(A)') '------  ----  ----  ---------'
!
!              DO I = 1, LISTSZ
!
!                 WRITE(*,'(3I6,I11)') NUMBER(I), END1(I), END2(I),
!             .             BRCKTI ( NUMBER(I), END1(I), END2(I) )
!
!              END DO
!
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Number  End1  End2  Bracketed
!        ------  ----  ----  ---------
!            -1     1    10          1
!            29     1    10         10
!             3    10   -10          3
!             3   -10    -1         -1
!
!
!     2) The following code example illustrates a typical use for
!        BRCKTI: force an identifier to be within a range. Note that
!        this code assumes that the user provided value is a valid
!        integer number.
!
!
!        Example code begins here.
!
!
!              PROGRAM BRCKTI_EX2
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER                 BRCKTI
!
!        C
!        C     Local parameters.
!        C
!              INTEGER                 KWDSZ
!              PARAMETER             ( KWDSZ = 30   )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(KWDSZ)       USRIN
!
!              INTEGER                 CODEIN
!              INTEGER                 CODEOK
!
!        C
!        C     Prompt the user for the code identifier.
!        C
!              CALL PROMPT ( 'Enter object code: ', USRIN )
!
!        C
!        C     Convert the user input to integer.
!        C
!              CALL PRSINT ( USRIN, CODEIN )
!
!        C
!        C     Object code must be in the range 701-705.
!        C
!              CODEOK = BRCKTI ( CODEIN, 701, 705 )
!
!        C
!        C     Display confirmation message.
!        C
!              IF ( CODEIN .NE. CODEOK ) THEN
!
!                 WRITE(*,'(A,I3,A)') 'Provided object code ', CODEIN,
!             .                       ' is out of range (701-705).'
!
!              ELSE
!
!                 WRITE(*,'(A,I3,A)') 'Provided object code ', CODEIN,
!             .                       ' is in range (701-705).'
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using '710' as user provided input, the output was:
!
!
!        Enter object code: 710
!        Provided object code 710 is out of range (701-705).
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
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 08-AUG-2021 (JDR) (BVS)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code examples based on existing code fragment.
!
!        Updated code fragment in $Particulars to show that the
!        order of endpoints is not important.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
!
!-&


! $ Index_Entries
!
!     bracket an integer value within an interval
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 30-DEC-1988 (WLT)
!
!        The routine was modified so that the order of the endpoints
!        of the bracketing interval is not needed. The routine now
!        determines which is the left endpoint and which is the
!        right and acts appropriately.
!
!-&


!
!     What else is there to say?
!
      if ( end1 .lt. end2 ) then

         brckti = max (  end1, min(end2,number)  )

      else

         brckti = max (  end2, min(end1,number)  )

      end if


      return
      end
