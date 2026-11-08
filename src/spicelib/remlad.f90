!$Procedure REMLAD (Remove elements from a double precision array)

      SUBROUTINE REMLAD ( NE, LOC, ARRAY, NA )

! $ Abstract
!
!     Remove one or more elements from a double precision array at the
!     indicated location.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!     ASSIGNMENT
!
! $ Declarations

      IMPLICIT NONE

      INTEGER          NE
      INTEGER          LOC
      DOUBLE PRECISION ARRAY ( * )
      INTEGER          NA

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NE         I   Number of elements to be removed.
!     LOC        I   Location of the first removed element.
!     ARRAY     I-O  Input/output array.
!     NA        I-O  Number of elements in the input/output array.
!
! $ Detailed_Input
!
!     NE       is the number of elements to be removed.
!
!     LOC      is the location in the array at which the first
!              element is to be removed.
!
!     ARRAY    on input, is the original array.
!
!     NA       on input, is the number of elements in ARRAY.
!
! $ Detailed_Output
!
!     ARRAY    on output, is the original array with elements
!              LOC through LOC+NE-1 removed. Succeeding elements
!              are moved forward to fill the vacated spaces.
!
!     NA       on output, is the number of elements in ARRAY.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If LOC is not in the interval [1, NA], the error
!         SPICE(INVALIDINDEX) is signaled.
!
!     2)  If the number of elements to be removed is greater than the
!         number of elements that can be removed, the error
!         SPICE(NONEXISTELEMENTS) is signaled.
!
!     3)  If NE is less than one, the array is not modified.
!
!     4)  If NA is less than one, any location is invalid, and, the
!         error SPICE(INVALIDINDEX) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The elements in positions LOC through LOC+NE-1 are overwritten
!     as the elements beginning at LOC+NE are moved back.
!
! $ Examples
!
!     Let
!
!           NA = 7      ARRAY(1) = 1.0D0
!                       ARRAY(2) = 2.0D0
!                       ARRAY(3) = 3.0D0
!                       ARRAY(4) = 4.0D0
!                       ARRAY(5) = 5.0D0
!                       ARRAY(6) = 6.0D0
!                       ARRAY(7) = 7.0D0
!
!     Then the call
!
!           CALL REMLAD ( 3, 3, ARRAY, NA )
!
!     yields the following result:
!
!           NA = 4      ARRAY(1) = 1.0D0
!                       ARRAY(2) = 2.0D0
!                       ARRAY(3) = 6.0D0
!                       ARRAY(4) = 7.0D0
!
!
!     The following calls would signal errors:
!
!     CALL REMLAD ( 3,  1, ARRAY, -1 )
!     CALL REMLAD ( 3, -1, ARRAY,  7 )
!     CALL REMLAD ( 3,  6, ARRAY,  7 )
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     remove elements from a d.p. array
!
!-&


! $ Revisions
!
!-     Beta Version 2.0.0, 1-JAN-1989 (HAN)
!
!         Code was added to handle the following exceptional
!         inputs.
!
!         If the dimension of the array is less than one, any
!         value of LOC is invalid. The old version did not check
!         the dimension of the array, and as a result, its output
!         was unpredictable.
!
!         If the location at which the elements are to be removed is
!         not in the interval [1, NA], an error is signaled.
!         Locations not within that interval refer to non-existent
!         array elements. The old routine did not signal an error.
!         It just returned the original array.
!
!         If the number of elements to be removed is greater than the
!         number of elements can be removed, an error is signaled.
!         In the old version, only those elements that could be
!         removed were removed, and no error was signaled.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL          RETURN


!
!     Local variables
!
      INTEGER          I



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'REMLAD' )
      END IF


!
!     If LOC does not point to an actual element, signal an error and
!     check out. If the dimension of the array is less than one, any
!     value of LOC is invalid, and an error is signaled.
!

      IF ( ( LOC .LT. 1 ) .OR. ( LOC .GT. NA ) ) THEN

         CALL SETMSG ( 'Location was *.'     )
         CALL ERRINT ( '*', LOC              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)' )
         CALL CHKOUT ( 'REMLAD' )
         RETURN

!
!     Don't try to remove non-existent elements.
!

      ELSE IF ( NE .GT. NA-LOC+1 ) THEN

         CALL SETMSG ( 'Trying to remove non-existent elements.' )
         CALL SIGERR ( 'SPICE(NONEXISTELEMENTS)' )
         CALL CHKOUT ( 'REMLAD' )
         RETURN


!
!     If there are elements to be removed, remove them. Otherwise,
!     do not modify the array.
!

      ELSE IF ( NE .GT. 0 ) THEN

!
!        Move the elements forward.
!
         DO I = LOC, NA - NE
            ARRAY(I) = ARRAY(I+NE)
         END DO


!
!        Update the number of elements in the array.
!
         NA = NA - NE


      END IF



      CALL CHKOUT ( 'REMLAD' )
      RETURN
      END
