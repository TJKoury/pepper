!$Procedure ODD ( Is a number odd? )

      logical function  odd ( ival )

! $ Abstract
!
!     Determine whether an integer is odd.
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
!     The function returns .TRUE. if IVAL is odd, otherwise .FALSE.
!
! $ Detailed_Input
!
!     IVAL     is the integer to be tested for oddness.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if IVAL is odd, .FALSE. if IVAL is
!     even.
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
!     Divide IVAL by two. If the remainder is one, IVAL is odd.
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
!     The following code fragment uses ODD to determine whether
!     an arbitrary value X is contained in any of the intervals.
!
!        CONTAINED = .FALSE.
!
!        DO I = 1, N-1
!           IF ( X .GE. ENDPTS(I)  .AND.  X .LE. ENDPTS(I+1) ) THEN
!              CONTAINED = ( ODD ( I ) )
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
!     B.V. Semenov       (JPL)
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
!-    SPICELIB Version 1.0.2, 07-NOV-2005 (BVS)
!
!        Fixed a few typos in the header.
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
!     test whether an integer is odd
!
!-&


!
!     Self-explanatory.
!
      odd = ( mod (ival,2) .ne. 0 )

      return
      end
