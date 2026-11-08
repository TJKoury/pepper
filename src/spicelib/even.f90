!$Procedure EVEN  ( Is an integer even? )

      logical function even ( ival )

! $ Abstract
!
!     Determine whether an integer is even.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     NUMBERS
!
! $ Declarations

      implicit none

      integer          ival

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IVAL       I   The integer in question.
!
!     The function returns .TRUE. if IVAL is even, otherwise .FALSE.
!
! $ Detailed_Input
!
!     IVAL     is the integer to be tested for evenness.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if IVAL is even, .FALSE. if IVAL is
!     odd.
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
!     Let ENDPTS contain a series of endpoints,
!
!        a , b , ..., a , b
!         1   1        n   n
!
!     representing an ordered collection of disjoint intervals,
!
!        a   <  b   < a
!         i  -   i     i+1
!
!     The following code fragment uses EVEN to determine whether
!     an arbitrary value X is contained in any of the intervals.
!
!        CONTAINED = .FALSE.
!
!        DO I = 1, N-1
!           IF ( X .GE. ENDPTS(I)  .AND.  X .LE. ENDPTS(I+1) ) THEN
!              CONTAINED = ( .NOT. EVEN ( I ) )
!           END IF
!        END DO
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
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Changed the input argument name "I" to "IVAL" for consistency
!        with other routines.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     test whether an integer is even
!
!-&


!
!     Self-explanatory.
!
      even = ( mod (ival,2) .eq. 0 )

      return
      end
