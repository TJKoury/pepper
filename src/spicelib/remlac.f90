!$Procedure REMLAC ( Remove elements from a character array )

      SUBROUTINE REMLAC ( NE, LOC, ARRAY, NA )

! $ Abstract
!
!     Remove one or more elements from a character array at the
!     indicated location.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
      CHARACTER*(*)    ARRAY ( * )
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
!           NA = 7      ARRAY(1) = 'The'
!                       ARRAY(2) = 'boy'
!                       ARRAY(3) = 'in'
!                       ARRAY(4) = 'the'
!                       ARRAY(5) = 'park'
!                       ARRAY(6) = 'fell'
!                       ARRAY(7) = 'down'
!
!     Then the call
!
!           CALL REMLAC ( 3, 3, ARRAY, NA )
!
!     yields the following result:
!
!           NA = 4      ARRAY(1) = 'The'
!                       ARRAY(2) = 'boy'
!                       ARRAY(3) = 'fell'
!                       ARRAY(4) = 'down'
!
!
!     The following calls would signal errors:
!
!     CALL REMLAC ( 3,  1, ARRAY, -1 )
!     CALL REMLAC ( 3, -1, ARRAY,  7 )
!     CALL REMLAC ( 3,  6, ARRAY,  7 )
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
!-    SPICELIB Version 1.1.0, 05-JUN-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     remove elements from a character array
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
         CALL CHKIN ( 'REMLAC' )
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
         CALL CHKOUT ( 'REMLAC' )
         RETURN

!
!     Don't try to remove non-existent elements.
!

      ELSE IF ( NE .GT. NA-LOC+1 ) THEN

         CALL SETMSG ( 'Trying to remove non-existent elements.' )
         CALL SIGERR ( 'SPICE(NONEXISTELEMENTS)' )
         CALL CHKOUT ( 'REMLAC' )
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



      CALL CHKOUT ( 'REMLAC' )
      RETURN
      END
