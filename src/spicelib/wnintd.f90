!$Procedure WNINTD ( Intersect two DP windows )

      SUBROUTINE WNINTD ( A, B, C )

! $ Abstract
!
!     Place the intersection of two double precision windows into
!     a third window.
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
!     WINDOWS
!
! $ Keywords
!
!     WINDOWS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      A        ( LBCELL:* )
      DOUBLE PRECISION      B        ( LBCELL:* )
      DOUBLE PRECISION      C        ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A,
!     B          I   Input windows.
!     C          O   Intersection of A and B.
!
! $ Detailed_Input
!
!     A,
!     B        are SPICE windows, each of which contains zero or more
!              intervals.
!
! $ Detailed_Output
!
!     C        is the output SPICE window, containing the
!              intersection of A and B --- every point contained in both
!              A and B.
!
!              C must be distinct from both A and B.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the intersection of the two windows results in an excess of
!         elements, the error SPICE(WINDOWEXCESS) is signaled.
!
!     2)  The cardinality of the input windows must be even. Left
!         endpoints of stored intervals must be strictly greater than
!         preceding right endpoints. Right endpoints must be greater
!         than or equal to corresponding left endpoints. Invalid window
!         data are not diagnosed by this routine and may lead to
!         unpredictable results.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The intersection of two windows contains every point contained
!     both in the first window and in the second window.
!
! $ Examples
!
!     Let A contain the intervals
!
!           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
!
!     and B contain the intervals
!
!           [ 2, 4 ]  [ 8, 10 ]  [ 16, 18 ]
!
!     Then the intersection of A and B contains the intervals
!
!           [ 2, 3 ]  [ 8, 10 ]
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Fixed I/O type
!        of argument C in $Brief_I/O table.
!
!        Added entry #2 in $Exceptions section.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     intersect two d.p. windows
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 27-FEB-1989  (HAN)
!
!        Due to the calling sequence and functionality changes
!        in the routine EXCESS, the method of signaling an
!        excess of elements needed to be changed.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDD
      INTEGER               SIZED
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               ACARD
      INTEGER               BCARD
      INTEGER               CSIZE

      INTEGER               AP
      INTEGER               BP
      INTEGER               CP
      CHARACTER*1           USE
      INTEGER               OVER



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WNINTD' )
      END IF

!
!     Find the cardinality of the input windows, and the allowed size
!     of the output window.
!
      ACARD = CARDD ( A )
      BCARD = CARDD ( B )
      CSIZE = SIZED ( C )

!
!     Begin with the input pointers at the first elements of the
!     input windows. The initial cardinality of the output window
!     is zero. And there is no overflow so far.
!
!     (Note that AP and BP point to the LEFT endpoints of intervals
!     in A and B, while CP points to the RIGHT endpoint of the latest
!     interval in C.)
!
      AP   = 1
      BP   = 1
      CP   = 0
      OVER = 0

!
!     When the end of either input window is reached, we're done.
!
      DO WHILE ( AP .LT. ACARD  .AND.  BP .LT. BCARD )

!
!        Let's see what we can do with the earlier of the next
!        intervals from A and B.
!
         IF ( A(AP+1) .LT. B(BP+1) ) THEN
            USE = 'A'

         ELSE IF ( B(BP+1) .LE. A(AP+1) ) THEN
            USE = 'B'
         END IF

!
!        If there is still space in the output window, fill it
!        as necessary. Otherwise, stop filling the array, but continue
!        to count the number of elements in excess of the size of the
!        output window.
!
!        The general idea is this: if the next interval of A overlaps
!        the next interval of B, save the area of overlap. (Similarly
!        for B.)
!
         IF ( USE .EQ. 'A' ) THEN

            IF ( A(AP+1) .GE. B(BP) ) THEN

               IF ( CP .LT. CSIZE ) THEN
                  CP      = CP + 2
                  C(CP-1) = MAX ( B(BP), A(AP) )
                  C(CP  ) = A(AP+1)
               ELSE
                  OVER    = OVER + 2
               END IF

            END IF

            AP = AP + 2

!
!        This is the same as the last clause, with B replacing A.
!
         ELSE IF ( USE .EQ. 'B' ) THEN

            IF ( B(BP+1) .GE. A(AP) ) THEN

               IF ( CP .LT. CSIZE ) THEN
                  CP      = CP + 2
                  C(CP-1) = MAX ( A(AP), B(BP) )
                  C(CP  ) = B(BP+1)
               ELSE
                  OVER    = OVER + 2
               END IF

            END IF

            BP = BP + 2

         END IF

      END DO

!
!     Set the cardinality of the output window.
!
      CALL SCARDD ( CP, C )

!
!     If there are any excess elements, signal an error and check out
!     as usual.
!
      IF ( OVER .GT. 0 ) THEN
         CALL EXCESS ( OVER, 'window' )
         CALL SIGERR ( 'SPICE(WINDOWEXCESS)' )
      END IF



      CALL CHKOUT ( 'WNINTD' )

      RETURN
      END
