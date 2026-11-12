!$Procedure      CHCKOI ( Check order of an d.p. array )

      SUBROUTINE CHCKOI ( NAME, ARRAY, ORDER, SIZE, OK )
      use pepper_legacy

! $ Abstract
!
!     Check the ordering of values in a d.p. array.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       TESTING
!
! $ Declarations
!
      IMPLICIT NONE
      CHARACTER*(*)         NAME
      INTEGER               ARRAY ( * )
      CHARACTER*(*)         ORDER
      INTEGER               SIZE
      LOGICAL               OK

!
! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      NAME       I   the name of the array to be examined.
!      ARRAY      I   the actual array
!      ORDER      I   the kind of order check to perform.
!      SIZE       I   the size of the input array
!      OK         O   TRUE if the test passes, FALSE otherwise..
!
! $ Detailed_Input
!
!     NAME        is the string used to give the name of an array.
!
!     ARRAY       is the actual integer array to be examined
!
!     ORDER       a string giving the kind of comparison to perform.
!                 Roughly speaking test that for I = 1, SIZE-1
!                 verify that the relation ARRAY(I) ORDER ARRAY(I+1)
!                 holds.  Allowed ORDER operators are:
!
!                 =   --- check that all values of the array are
!                         the same.
!
!                 <   --- check that the values in the array are
!                         strictly increasing.
!
!                 >   --- check that the values in the array are
!                         strictly decreasing.
!
!                 =>  --- check that the values in the array are
!                         non-increasing (values can be duplicated
!                         but can't get bigger.)
!
!                 <=  --- check that the values in the array are
!                         non-decreasing (values can be duplicated
!                         but can't get smaller.)
!
!     SIZE        is the number of elements in the array.
!
! $ Detailed_Output
!
!     OK          if the check of the input array is successful then
!                 OK is given the value TRUE.  Otherwise OK is given the
!                 value FALSE and a diagnostic message is sent to the
!                 test logger.
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     Error free.
!
!
! $ Files
!
!      None.
!
! $ Particulars
!
!     This routine checks that the items in an array satisfy an
!     order relationship.
!
! $ Examples
!
!     Suppose that you have a routine that orders the items in
!     an array so that they are increasing.  To test this routine
!     you could make the following calls:
!
!        CALL ORDER ( ARRAY, SIZE )
!        CALL ORDER ( 'ARRAY', ARRAY, '<', SIZE, OK )
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
!     W.L. Taber      (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    Testing Utilities 1.0.1, 05-AUG-2013 (BVS)
!
!        Corrected index entry.
!
!-    Testing Utilities 1.0.0, 18-NOV-1994 (WLT)
!
!
!-&
!
! $ Index_Entries
!
!     Check the order of elements of an integer array.
!
!-&
!
!     SPICELIB Functions
!
!
!     Test Utility Functions
!
      LOGICAL               VERBOS
!
!     Local Variables
!

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 240 )

      CHARACTER*(LNSIZE)    MESSGE

      INTEGER               I
      INTEGER               J
      INTEGER               R1
      INTEGER               R2

      LOGICAL               FAIL

      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )

      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD

      CALL TSTSTY ( GOOD, BAD )
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ', &
                    'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )




!
!     So far the test case has not failed.
!
      FAIL = .FALSE.
      J    =  SIZE + 1


      IF ( ORDER .EQ. '=' ) THEN

         DO I = 1, SIZE-1
            FAIL = FAIL .OR. .NOT. ( ARRAY(I) .EQ. ARRAY(I+1) )
         END DO

         IF ( FAIL ) THEN
            J = MIN ( J, I )
         END IF

      ELSE IF ( ORDER .EQ. '<' ) THEN

         DO I = 1, SIZE-1
            FAIL = FAIL .OR. .NOT. ( ARRAY(I) .LT. ARRAY(I+1) )
         END DO

         IF ( FAIL ) THEN
            J = MIN ( J, I )
         END IF

      ELSE IF ( ORDER .EQ. '<=' ) THEN

         DO I = 1, SIZE-1
            FAIL = FAIL .OR. .NOT. ( ARRAY(I) .LE. ARRAY(I+1) )
         END DO

         IF ( FAIL ) THEN
            J = MIN ( J, I )
         END IF

      ELSE IF ( ORDER .EQ. '>' ) THEN

         DO I = 1, SIZE-1
            FAIL = FAIL .OR. .NOT. ( ARRAY(I) .GT. ARRAY(I+1) )
         END DO

         IF ( FAIL ) THEN
            J = MIN ( J, I )
         END IF

      ELSE IF ( ORDER .EQ. '=>' ) THEN

         DO I = 1, SIZE-1
            FAIL = FAIL .OR. .NOT. ( ARRAY(I) .GE. ARRAY(I+1) )
         END DO

         IF ( FAIL ) THEN
            J = MIN ( J, I )
         END IF

      END IF


      R1 = RTRIM(NAME)
      R2 = RTRIM(ORDER)

      IF ( FAIL ) THEN

         MESSGE = 'The input array # is does not satisfy the ' &
         //       'condition "#(@) ? #($)".  The two offending ' &
         //       'values are:/cr/cr(3:) #(@) = % /cr #($) = &. '

         CALL REPMC ( MESSGE, '#', NAME(1:R1),       MESSGE )
         CALL REPMC ( MESSGE, '#', NAME(1:R1),       MESSGE )
         CALL REPMC ( MESSGE, '#', NAME(1:R1),       MESSGE )
         CALL REPMC ( MESSGE, '#', NAME(1:R1),       MESSGE )
         CALL REPMC ( MESSGE, '#', NAME(1:R1),       MESSGE )
         CALL REPMC ( MESSGE, '?', ORDER(1:R2),      MESSGE )
         CALL REPMI ( MESSGE, '@', J,                MESSGE )
         CALL REPMI ( MESSGE, '@', J,                MESSGE )
         CALL REPMI ( MESSGE, '$', J+1,              MESSGE )
         CALL REPMI ( MESSGE, '$', J+1,              MESSGE )
         CALL REPMI ( MESSGE, '%', ARRAY(J),         MESSGE )
         CALL REPMI ( MESSGE, '&', ARRAY(J+1),       MESSGE )
         CALL TSTLOG( ' ',     FAIL                         )
         CALL TSTLOG( MESSGE,  FAIL                         )

      ELSE IF ( VERBOS() ) THEN

         MESSGE = 'The relationship #(I) ? #(I+1) is true for ' &
         //       'all elements of the input array # '

         CALL REPMC ( MESSGE, '#', NAME(1:R1),       MESSGE )
         CALL REPMC ( MESSGE, '#', NAME(1:R1),       MESSGE )
         CALL REPMC ( MESSGE, '#', NAME(1:R1),       MESSGE )
         CALL REPMC ( MESSGE, '?', ORDER(1:R2),      MESSGE )
         CALL TSTLOG( ' ',     FAIL                         )
         CALL TSTLOG( MESSGE,  FAIL                         )

      END IF

      CALL TSTLGS ( GOOD, BAD )
      OK = .NOT. FAIL

      RETURN
      END



