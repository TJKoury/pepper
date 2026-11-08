!$Procedure      M2SHLL ( Shell sort an array of Meta/2 syntaxs )

      SUBROUTINE M2SHLL ( NDIM, ARRAY )

! $ Abstract
!
!      Sort an array of character strings according suitable for
!      use with META/2.
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
!      ARRAY,  SORT
!
! $ Declarations

      IMPLICIT NONE
      INTEGER          NDIM
      CHARACTER*(*)    ARRAY ( * )

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      NDIM       I   Dimension of the array.
!      ARRAY     I/O  The array of syntax statements
!
! $ Detailed_Input
!
!      NDIM        is the number of elements in the array to be sorted.
!
!      ARRAY       on input, is the array of syntax statements
!                  to be sorted.
!
! $ Detailed_Output
!
!      ARRAY       on output, contains the same elements, sorted
!                  by initial keyword minus any keyword labels.
!
! $ Parameters
!
!     None.
!
! $ Particulars
!
!      The Shell Sort Algorithm is well known.
!
! $ Examples
!
!      Let ARRAY contain the following elements:
!
!            'FEYNMAN'
!            'NEWTON'
!            'EINSTEIN'
!            'GALILEO'
!            'EUCLID'
!            'Galileo'
!
!      Then after a call to M2SHLL, the array would be ordered as
!      follows:
!
!            'EINSTEIN'
!            'EUCLID'
!            'FEYNMAN'
!            'GALILEO'
!            'Galileo'
!            'NEWTON'
!
! $ Restrictions
!
!      None.
!
! $ Exceptions
!
!      Error free.
!
! $ Files
!
!      None.
!
! $ Author_and_Institution
!
!      I.M. Underwood  (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-     Meta/2  Version 1.0.0, 4-SEP-1998 (WLT)
!
!         Modified SHELLC to produce an initial keyword sort.
!
!-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&

! $ Index_Entries
!
!     shell sort a character array
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               POS

!
!     Local variables
!
      INTEGER               GAP
      INTEGER               EJ
      INTEGER               EJG
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               KG
      INTEGER               JG

      LOGICAL               SWAP



!
!     This is a straightforward implementation of the Shell Sort
!     algorithm.
!
      GAP = NDIM / 2

      DO WHILE ( GAP .GT. 0 )

         DO I = GAP+1, NDIM

            J = I - GAP
            DO WHILE ( J .GT. 0 )
               JG = J + GAP
               K  = J
               KG = JG

               EJ  = POS(ARRAY(J), '[', 1 )
               EJG = POS(ARRAY(JG),'[', 1 )

               IF ( EJ .GT. 1 ) THEN
                  ARRAY(J)(EJ:EJ) = ' '
               END IF

               IF ( EJG .GT. 1 ) THEN
                  ARRAY(JG)(EJG:EJG) = ' '
               END IF


               IF ( LLE (ARRAY(J), ARRAY(JG)) ) THEN
                  J    = 0
                  SWAP = .FALSE.
               ELSE
                  SWAP = .TRUE.
               END IF

               IF ( EJ .GT. 1 ) THEN
                  ARRAY(K)(EJ:EJ) = '['
               END IF

               IF ( EJG .GT. 1 ) THEN
                  ARRAY(KG)(EJG:EJG) = '['
               END IF

               IF ( SWAP ) THEN
                  CALL SWAPC ( ARRAY(J), ARRAY(JG) )
               END IF


               J = J - GAP
            END DO

         END DO

         GAP = GAP / 2

      END DO

      RETURN
      END
