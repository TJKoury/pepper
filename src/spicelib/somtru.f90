!$Procedure SOMTRU ( Some entries true? )

      logical function somtru ( logcls, n )

! $ Abstract
!
!     Determine if some of the entries in an array of logicals are
!     .TRUE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      implicit none

      logical               logcls ( * )
      integer               n

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LOGCLS     I   An array of logicals.
!     N          I   Number of elements in the array LOGCLS.
!
!     The function returns .TRUE. if some of the values in the array
!     LOGCLS are .TRUE.
!
! $ Detailed_Input
!
!     LOGCLS   is an array of logicals.
!
!     N        is the number of elements in the array LOGCLS
!
! $ Detailed_Output
!
!     The function returns true if the value of some entry of LOGCLS
!     is .TRUE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If N is less than 1, the function returns a value of .FALSE.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This function examines each element of LOGCLS until
!     a .TRUE. value is found or until all values have been
!     examined.
!
! $ Examples
!
!     Suppose you needed to confirm that a character set
!     WORDS contained at least one of the words in the phrase
!
!       'EVERY GOOD BOY DOES FINE'
!
!     You might execute the following block of code.
!
!           FOUND(1) = ELEMC  ( 'EVERY', WORDS )
!           FOUND(2) = ELEMC  ( 'GOOD',  WORDS )
!           FOUND(3) = ELEMC  ( 'BOY',   WORDS )
!           FOUND(4) = ELEMC  ( 'DOES',  WORDS )
!           FOUND(5) = ELEMC  ( 'FINE',  WORDS )
!
!           OK       = SOMTRU ( FOUND,   5     )
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
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 12-JUL-1991 (WLT)
!
!-&


! $ Index_Entries
!
!     test whether some logicals in an array are true
!
!-&


!
!     Local variables
!
      integer               i

!
!     Just do it.
!
      do i = 1, n

         if ( logcls(i) ) then

            somtru = .true.
            return

         end if

      end do

      somtru = .false.
      return


      end
