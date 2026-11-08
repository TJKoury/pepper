!$Procedure COPYI ( Copy an integer cell )

      SUBROUTINE COPYI ( CELL, COPY )

! $ Abstract
!
!     Copy the contents of an integer cell to another cell.
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
!     CELLS
!
! $ Keywords
!
!     CELLS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER            CELL     ( LBCELL:* )
      INTEGER            COPY     ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CELL       I   Cell to be copied.
!     COPY       O   New cell.
!
! $ Detailed_Input
!
!     CELL     is a cell.
!
! $ Detailed_Output
!
!     COPY     is a cell which contains the same elements as the
!              input cell, in the same order. If the size (maximum
!              cardinality) of the output cell is smaller than
!              the cardinality of the input cell, then only as many
!              items as will fit in the output cell are copied,
!              and an error is signaled.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the output cell in not large enough to hold the elements
!         of the input cell, the error SPICE(CELLTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The copy routines (COPYC, COPYD, and COPYI) are used primarily
!     to manipulate working cells, since many routines that use cells
!     (binary set routines, for instance) do not allow cells to be
!     combined or manipulated in place.
!
! $ Examples
!
!     In the following example, COPYC is used to copy the result
!     of the union of two sets (ordered cells) from a temporary
!     working set back into the one of the original set.
!
!           CALL UNIONC ( BODIES, PLANETS, TEMP )
!           CALL COPYC  ( TEMP,   BODIES        )
!
!     If the size of the temporary cell is greater than the size
!     of the original set, the function FAILED should be checked to be
!     sure that no overflow occurred. If BODIES is at least as
!     large as TEMP, no such check is necessary.
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
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     copy an integer cell
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 09-JAN-1989 (NJB)
!
!        Calling protocol for EXCESS has been changed. Call to SETMSG
!        has been removed.
!
!-&


!
!     SPICELIB functions
!
      INTEGER          SIZEI
      INTEGER          CARDI
      LOGICAL          RETURN

!
!     Local variables
!
      INTEGER          SIZE
      INTEGER          CARD
      INTEGER          MOVED
      INTEGER          I


!
!     Set up the error processing.
!
      IF ( RETURN() ) RETURN
      CALL CHKIN ( 'COPYI' )

!
!     We need the cardinality of the input cell, and the size of
!     the output cell.
!
      CARD = CARDI ( CELL )
      SIZE = SIZEI ( COPY )

!
!     Start moving the elements, one by one. Stop if the output
!     cell fills up.
!
      MOVED = MIN ( CARD, SIZE )

      DO I = 1, MOVED
         COPY(I) = CELL(I)
      END DO

!
!     Set the cardinality of the output cell. Report any excess.
!
      CALL SCARDI ( MOVED, COPY )

      IF ( CARD .GT. SIZE ) THEN

         CALL EXCESS ( CARD - SIZE, 'cell' )
         CALL SIGERR ( 'SPICE(CELLTOOSMALL)' )
         CALL CHKOUT ( 'COPYI' )
         RETURN

      END IF

      CALL CHKOUT ( 'COPYI' )
      RETURN
      END
