!$Procedure INSLAD (Insert at location in double precision array)

      SUBROUTINE INSLAD ( ELTS, NE, LOC, ARRAY, NA )

! $ Abstract
!
!     Insert one or more elements into a double precision array at
!     the indicated location.
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

      DOUBLE PRECISION      ELTS  ( * )
      INTEGER               NE
      INTEGER               LOC
      DOUBLE PRECISION      ARRAY ( * )
      INTEGER               NA

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ELTS       I   Elements to be inserted.
!     NE         I   Number of elements to be inserted.
!     LOC        I   Location of the first inserted element.
!     ARRAY     I-O  Input/output array.
!     NA        I-O  Number of elements in the input/output array.
!
! $ Detailed_Input
!
!     ELTS     contains one or more elements which are to be
!              inserted into the input array.
!
!     NE       is the number of elements to be inserted.
!
!     LOC      is the location in the array at which the first
!              element of ELTS is to be inserted. LOC must be
!              within the interval [1, NA+1]. To append to
!              ARRAY, set LOC equal to NA+1.
!
!     ARRAY    on input, is the original array.
!
!     NA       on input, is the number of elements in ARRAY.
!
! $ Detailed_Output
!
!     ARRAY    on output, is the original array with the elements
!              of ELT inserted into positions LOC through LOC+NE-1.
!              The original elements in these positions are moved
!              back to make room for the inserted elements.
!
!     NA       on output, is the number of elements in ARRAY.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  The dimension of the array is set equal to zero if its
!         input value is less than one.
!
!     2)  If LOC is not in the interval [1, NA+1], the error
!         SPICE(INVALIDINDEX) is signaled.
!
!     3)  If the number of elements to be inserted is less than one,
!         the array is not modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The elements in positions LOC through LOC+NE-1 are moved back
!     by NE spaces to make room for the new elements, which are then
!     inserted into the vacated spaces.
!
! $ Examples
!
!     Let
!
!           ELTS(1) = 5.0D0        NA = 4      ARRAY(1) =  1.0D0
!           ELTS(2) = 6.0D0                    ARRAY(2) =  2.0D0
!           ELTS(3) = 7.0D0                    ARRAY(3) =  3.0D0
!                                              ARRAY(4) =  4.0D0
!
!     Then the call
!
!           CALL INSLAD ( ELTS, 3, 3, ARRAY, NA )
!
!     yields the following result:
!
!           NA = 7      ARRAY(1) = 1.0D0
!                       ARRAY(2) = 2.0D0
!                       ARRAY(3) = 5.0D0
!                       ARRAY(4) = 6.0D0
!                       ARRAY(5) = 7.0D0
!                       ARRAY(6) = 3.0D0
!                       ARRAY(7) = 4.0D0
!
!
!     The following calls to INSLAD signal errors.
!
!     CALL INSLAD ( ELTS, 3, -1, ARRAY, NA )
!     CALL INSLAD ( ELTS, 3,  6, ARRAY, NA )
!     CALL INSLAD ( ELTS, 3,  2, ARRAY, -1 )
!     CALL INSLAD ( ELTS, 3, -1, ARRAY, -1 )
!
! $ Restrictions
!
!     1)  The array must be large enough to contain both the original
!         and the inserted elements.
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
!     insert at location in d.p. array
!
!-&


! $ Revisions
!
!-     Beta Version 2.0.0, 30-DEC-1988 (HAN)
!
!         If the location at which the elements are to be inserted is
!         not in the interval [1, NA+1], an error is signaled.
!         Locations not within that interval refer to non-existent
!         array elements. (To append to the array, the location
!         should be equal to NA+1.)
!
!         A negative dimension bug was fixed. The results of the
!         old version were unpredictable if the input array dimension
!         was negative. To avoid this problem the maximum of zero and
!         the input dimension becomes the dimension used by the
!         the routine. In this case, the only valid location at which
!         to insert is 1. If it is not 1, an error is signaled
!         when the location is checked.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL          RETURN


!
!     Other functions
!
      INTEGER          MAX

!
!     Local variables
!
      INTEGER          SIZE
      INTEGER          I




!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'INSLAD' )
      END IF


!
!     Check the dimension of the array.
!
      SIZE = MAX ( 0, NA )

!
!     Make sure the location at which the elements are to be inserted
!     is not out of range. If it is, signal an error and bail out.
!

      IF ( ( LOC .LT. 1 ) .OR. ( LOC .GT. SIZE+1 ) ) THEN

         CALL SETMSG ( 'Location was *.'     )
         CALL ERRINT ( '*', LOC              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)' )
         CALL CHKOUT ( 'INSLAD' )
         RETURN

      END IF


!
!     If the number of elements to be inserted is greater than zero,
!     insert them. If not, do not modify the array.
!
      IF ( NE .GT. 0 ) THEN

!
!        Move the trailing elements back to make room for the new ones.
!
         DO I = SIZE, LOC, -1
            ARRAY(I+NE) = ARRAY(I)
         END DO

!
!        Now put the new elements in the vacated spaces.
!
         DO I = 1, NE
            ARRAY(LOC+I-1) = ELTS(I)
         END DO

!
!        Update the number of elements in the array.
!
         NA = SIZE + NE


      END IF



      CALL CHKOUT ( 'INSLAD' )
      RETURN
      END
