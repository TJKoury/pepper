!$Procedure EQSTR ( Equivalent strings )

      logical function eqstr ( a, b )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Determine whether two strings are equivalent.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ALPHANUMERIC
!     ASCII
!     CHARACTER
!     COMPARE
!     PARSING
!     SEARCH
!     STRING
!     TEXT
!
! $ Declarations

      implicit none

      character*(*)         a
      character*(*)         b

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A,
!     B          I   Arbitrary character strings.
!
!     The function returns .TRUE. if A and B are equivalent.
!
! $ Detailed_Input
!
!     A,
!     B        are arbitrary character strings.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if A and B are equivalent: that is,
!     if A and B contain  the same characters in the same order,
!     when blanks are ignored and uppercase and lowercase characters
!     are considered equal.
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
!     This routine is provided for those cases in which two strings
!     must be compared, and in which allowances are to be made for
!     extra (leading, trailing, and embedded) blanks and differences
!     in case. For the most part,
!
!        EQSTR ( A, B )
!
!     is .TRUE. whenever
!
!        CALL CMPRSS ( ' ', 0, A, TEMPA )
!        CALL UCASE  (            TEMPA, TEMPA )
!
!        CALL CMPRSS ( ' ', 0, B, TEMPB )
!        CALL UCASE  (            TEMPB, TEMPB )
!
!        EQVLNT = TEMPA .EQ. TEMPB
!
!     is .TRUE. There are two important differences, however.
!
!        1) The single reference to EQSTR is much simpler to
!           write, and simpler to understand.
!
!        2) The reference to EQSTR does not require any temporary
!           storage, nor does it require that the strings A and B
!           be changed. This feature is especially useful when
!           comparing strings received as subprogram arguments
!           against strings stored internally within the subprogram.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) This code provides examples of equivalent and non-equivalent
!        strings according to the algorithm implemented in EQSTR.
!
!        Example code begins here.
!
!
!              PROGRAM EQSTR_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              LOGICAL               EQSTR
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               SETSIZ
!              PARAMETER           ( SETSIZ = 9  )
!
!              INTEGER               STRLEN
!              PARAMETER           ( STRLEN = 22 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(STRLEN)    STR1   ( SETSIZ )
!              CHARACTER*(STRLEN)    STR2   ( SETSIZ )
!
!              INTEGER               I
!
!        C
!        C     Initialize the two arrays of strings.
!        C
!              DATA                  STR1   / 'A short string   ',
!             .                               'Embedded        blanks',
!             .                               'Embedded        blanks',
!             .                               ' ',
!             .                               'One word left out',
!             .                               'Extra [] delimiters',
!             .                               'Testing 1, 2, 3',
!             .                               'Case insensitive',
!             .                               'Steve'  /
!
!              DATA                  STR2   / 'ashortstring',
!             .                               'Em be dd ed bl an ks',
!             .                               '   Embeddedblanks',
!             .                               '          ',
!             .                               'WORD LEFT OUT',
!             .                               'extradelimiters',
!             .                               'TESTING123',
!             .                               'Case Insensitive',
!             .                               '  S t E v E  '  /
!
!
!        C
!        C     Compare the two arrays.
!        C
!              DO I = 1, SETSIZ
!
!                 WRITE(*,*)
!                 WRITE(*,*) 'STR1 : ', STR1(I)
!                 WRITE(*,*) 'STR2 : ', STR2(I)
!
!                 IF ( EQSTR( STR1(I), STR2(I) ) ) THEN
!
!                    WRITE(*,*) 'EQSTR: equivalent.'
!
!                 ELSE
!
!                    WRITE(*,*) 'EQSTR: NOT equivalent.'
!
!                 END IF
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         STR1 : A short string
!         STR2 : ashortstring
!         EQSTR: equivalent.
!
!         STR1 : Embedded        blanks
!         STR2 : Em be dd ed bl an ks
!         EQSTR: equivalent.
!
!         STR1 : Embedded        blanks
!         STR2 :    Embeddedblanks
!         EQSTR: equivalent.
!
!         STR1 :
!         STR2 :
!         EQSTR: equivalent.
!
!         STR1 : One word left out
!         STR2 : WORD LEFT OUT
!         EQSTR: NOT equivalent.
!
!         STR1 : Extra [] delimiters
!         STR2 : extradelimiters
!         EQSTR: NOT equivalent.
!
!         STR1 : Testing 1, 2, 3
!         STR2 : TESTING123
!         EQSTR: NOT equivalent.
!
!         STR1 : Case insensitive
!         STR2 : Case Insensitive
!         EQSTR: equivalent.
!
!         STR1 : Steve
!         STR2 :   S t E v E
!         EQSTR: equivalent.
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
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing example fragments.
!
!-    SPICELIB Version 1.2.0, 03-AUG-1994 (NJB)
!
!        Code changed to eliminate DO WHILE ( .TRUE. ) construct.
!        The purpose of the change was to eliminate compilation
!        diagnostics relating to unreachable statements. The code
!        ran just fine before this change.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 10-MAY-1990 (NJB)
!
!        Loop termination condition fixed.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     equivalent strings
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0, 03-AUG-1994 (NJB)
!
!        Code changed to eliminate DO WHILE ( .TRUE. ) construct.
!        The purpose of the change was to eliminate compilation
!        diagnostics relating to unreachable statements.
!
!        Changed some statements of form
!
!           IF <condition> <statement>
!
!        to
!
!           IF <condition> THEN
!
!             <statement>
!
!           END IF
!
!-    SPICELIB Version 1.1.0, 10-MAY-1990 (NJB)
!
!        Loop termination condition fixed. The routine now checks
!        the termination case where both string pointers are pointing
!        to blanks, and at least one pointer has a value greater than
!        the length of the string it corresponds to. Internal comments
!        were updated accordingly.
!
!-&


!
!     Local variables
!
      integer(ip)           pa
      integer(ip)           pb

      integer(ip)           lena
      integer(ip)           lenb

      integer(ip)           ca
      integer(ip)           cb

      integer(ip)           lbound
      integer(ip)           ubound
      integer(ip)           delta

      logical               done


!
!     The general plan is to move a pair of pointers (PA, PB)
!     through strings A and B, skipping blank characters and
!     comparing others one-for-one.
!
!        Repeat:
!
!           If (A is blank) then
!              Increment A
!
!           Else if (B is blank) then
!              Increment B
!
!           Else
!              If (A and B are equivalent) then
!                 Increment A and B
!              Else
!                 Return .FALSE.
!
!           If (A and B are past end) then
!              Return .TRUE.
!
!           Else if (A or B is past end and other is non-blank) then
!              Return .FALSE.
!
!           Else if (A or B is past end and other is blank) then
!              Return .TRUE.
!
!     Note that no pointer gets incremented more than once on each
!     pass through the loop.
!
!     On the other hand, in many cases the strings will be exactly
!     equal. If so, why knock ourselves out?
!
      if ( a .eq. b ) then

         eqstr  = .true.
         return

      else

         pa = 1
         pb = 1

         lena = len ( a )
         lenb = len ( b )

         lbound = ichar ( 'a' )
         ubound = ichar ( 'z' )
         delta  = ichar ( 'A' ) - ichar ( 'a' )

         done   = .false.

         do while ( .not. done )

!
!           At this point, we're guaranteed that
!
!             ( PA .LE. LENA )   and   ( PB .LE. LENB )
!

            if ( a(pa:pa) .eq. ' ' ) then
               pa = pa + 1

            else if ( b(pb:pb) .eq. ' ' ) then
               pb = pb + 1

            else

               ca = ichar ( a(pa:pa) )
               cb = ichar ( b(pb:pb) )

               if ( ca .ge. lbound  .and.  ca .le. ubound ) then
                  ca = ca + delta
               end if

               if ( cb .ge. lbound  .and.  cb .le. ubound ) then
                  cb = cb + delta
               end if

               if ( ca .eq. cb ) then

                  pa = pa + 1
                  pb = pb + 1

               else

                  eqstr  = .false.
                  done   = .true.
!
!                 We'll return from this point, having taken no further
!                 action.
!
               end if

            end if


            if ( .not. done ) then


               if ( pa .gt. lena ) then
!
!                 Whichever of the following tests passes, we're going
!                 to have a verdict at the end of the IF block below.
!
                  if ( pb .gt. lenb ) then
                     eqstr  = .true.

                  else if ( b(pb: ) .ne. ' ' ) then
                     eqstr  = .false.

                  else
                     eqstr  = .true.

                  end if

                  done  =  .true.
!
!                 We'll return from this point, having taken no further
!                 action.
!

               else if ( pb .gt. lenb ) then
!
!                 Whichever of the following tests passes, we're going
!                 to have a verdict at the end of the IF block below.
!
                  if ( a(pa: ) .ne. ' ' ) then
                     eqstr  = .false.
                  else
                     eqstr  = .true.
                  end if

                  done  =  .true.
!
!                 We'll return from this point, having taken no further
!                 action.
!
               end if


            end if


         end do


      end if

      end
