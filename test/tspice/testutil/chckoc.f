C$Procedure      CHCKOC ( Check order of an d.p. array )

      SUBROUTINE CHCKOC ( NAME, ARRAY, ORDER, SIZE, OK )

C$ Abstract
C
C     Check the ordering of values in a d.p. array.
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
C      None.
C
C$ Keywords
C
C       TESTING
C
C$ Declarations
C
      IMPLICIT NONE
      CHARACTER*(*)         NAME
      CHARACTER*(*)         ARRAY ( * )
      CHARACTER*(*)         ORDER
      INTEGER               SIZE
      LOGICAL               OK
 
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NAME       I   the name of the array to be examined.
C      ARRAY      I   the actual array
C      ORDER      I   the kind of order check to perform.
C      SIZE       I   the size of the input array
C      OK         O   TRUE if the test passes, FALSE otherwise..
C
C$ Detailed_Input
C
C     NAME        is the string used to give the name of an array.
C
C     ARRAY       is the actual string array to be examined
C
C     ORDER       a string giving the kind of comparison to perform.
C                 Roughly speaking test that for I = 1, SIZE-1
C                 verify that the relation ARRAY(I) ORDER ARRAY(I+1)
C                 holds.  Allowed ORDER operators are:
C
C                 =   --- check that all values of the array are
C                         the same.
C
C                 <   --- check that the values in the array are
C                         strictly increasing.
C
C                 >   --- check that the values in the array are
C                         strictly decreasing.
C
C                 =>  --- check that the values in the array are
C                         non-increasing (values can be duplicated
C                         but can't get bigger.)
C
C                 <=  --- check that the values in the array are
C                         non-decreasing (values can be duplicated
C                         but can't get smaller.)
C
C     SIZE        is the number of elements in the array.
C
C$ Detailed_Output
C
C     OK          if the check of the input array is successful then
C                 OK is given the value TRUE.  Otherwise OK is given the
C                 value FALSE and a diagnostic message is sent to the
C                 test logger.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This routine checks that the items in an array satisfy an
C     order relationship.
C
C$ Examples
C
C     Suppose that you have a routine that orders the items in
C     an array so that they are increasing.  To test this routine
C     you could make the following calls:
C
C        CALL ORDER ( ARRAY, SIZE )
C        CALL ORDER ( 'ARRAY', ARRAY, '<', SIZE, OK )
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
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    Testing Utilities 1.0.1, 05-AUG-2013 (BVS)
C
C        Corrected index entry.
C
C-    Testing Utilities 1.0.0, 18-NOV-1994 (WLT)
C
C
C-&
C
C$ Index_Entries
C
C     Check the order of elements of a character array.
C
C-&
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
C
C     Test Utility Functions
C
      LOGICAL               VERBOS
C
C     Local Variables
C
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 800 )
 
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
C
C     So far the test case has not failed.
C
      FAIL = .FALSE.
      J    =  SIZE + 1
 
      IF ( ORDER .EQ. '=' ) THEN
 
         DO I = 1, SIZE-1
            FAIL = FAIL .OR. .NOT. ( ARRAY(I) .EQ. ARRAY(I+1) )
 
            IF ( FAIL ) THEN
               J = MIN ( J, I )
            END IF
 
         END DO
 
      ELSE IF ( ORDER .EQ. '<' ) THEN
 
         DO I = 1, SIZE-1
            FAIL = FAIL .OR. .NOT. LLT ( ARRAY(I), ARRAY(I+1) )
         END DO
 
         IF ( FAIL ) THEN
            J = MIN ( J, I )
         END IF
 
      ELSE IF ( ORDER .EQ. '<=' ) THEN
 
         DO I = 1, SIZE-1
            FAIL = FAIL .OR. .NOT. LLE ( ARRAY(I), ARRAY(I+1) )
         END DO
 
         IF ( FAIL ) THEN
            J = MIN ( J, I )
         END IF
 
      ELSE IF ( ORDER .EQ. '>' ) THEN
 
         DO I = 1, SIZE-1
            FAIL = FAIL .OR. .NOT. LGT ( ARRAY(I), ARRAY(I+1) )
         END DO
 
         IF ( FAIL ) THEN
            J = MIN ( J, I )
         END IF
 
      ELSE IF ( ORDER .EQ. '=>' ) THEN
 
         DO I = 1, SIZE-1
            FAIL = FAIL .OR. .NOT. LGE ( ARRAY(I), ARRAY(I+1) )
         END DO
 
         IF ( FAIL ) THEN
            J = MIN ( J, I )
         END IF
 
      END IF
 
 
      R1 = RTRIM(NAME)
      R2 = RTRIM(ORDER)
 
      CALL TSTSTY ( GOOD, BAD )
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ',
     .              'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )
 
      IF ( FAIL ) THEN
 
         MESSGE = 'The input array # is does not satisfy the '
     .   //       'condition "#(@) ? #($)".  The two offending '
     .   //       'values are:/cr/cr(3:) #(@) = "%" /cr #($) = "&". '
 
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
         CALL REPMC ( MESSGE, '%', ARRAY(J),         MESSGE )
         CALL REPMC ( MESSGE, '&', ARRAY(J+1),       MESSGE )
         CALL TSTLOG( MESSGE,  FAIL                         )
 
      ELSE IF ( VERBOS() ) THEN
 
         MESSGE = 'The relationship #(I) ? #(I+1) is true for '
     .   //       'all elements of the input array # '
 
         CALL REPMC ( MESSGE, '#', NAME(1:R1),       MESSGE )
         CALL REPMC ( MESSGE, '#', NAME(1:R1),       MESSGE )
         CALL REPMC ( MESSGE, '#', NAME(1:R1),       MESSGE )
         CALL REPMC ( MESSGE, '?', ORDER(1:R2),      MESSGE )
         CALL TSTLOG( MESSGE,  FAIL                         )
 
      END IF
 
      CALL TSTLGS ( GOOD, BAD )
      OK = .NOT. FAIL
 
      RETURN
      END
 
 
 
