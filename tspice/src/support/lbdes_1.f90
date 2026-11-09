
!$Procedure LBDES ( Line buffer, describe )

      SUBROUTINE LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )
      IMPLICIT NONE

! $ Abstract
!
!     Describe the current internal status of a line buffer.
!
! $ Required_Reading
!
!     CB, LB
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     STRING
!     TEXT
!
! $ Declarations

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               PTRS         ( LBCELL:* )
      INTEGER               MAXLN
      INTEGER               NLINE
      INTEGER               NCOM
      INTEGER               PCARD

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     PTRS       I   Pointer component of the buffer.
!     MAXLN      O   Maximum number of lines.
!     NLINE      O   Current number of lines.
!     NCOM       O   Current number of complement intervals.
!     PCARD      O   Current cardinality of PTRS.
!
! $ Detailed_Input
!
!     PTRS        is the pointer component of a line buffer.
!
! $ Detailed_Output
!
!     MAXLN       is the maximum number of lines that can be stored in
!                 the buffer at any one time.
!
!     NLINE       is the number of lines currently stored in the buffer.
!
!     NCOM        is the number of complement intervals (contiguous
!                 spaces in which new lines can be stored) currently
!                 available in the buffer.
!
!     PCARD       is the current cardinality of PTRS.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Particulars
!
!     This routine is intended primarily for internal use by the
!     line buffer routines. However, the information that it returns
!     can be useful for error checking and debugging purposes.
!
! $ Examples
!
!     In the following code fragment, a check is performed before
!     attempting to use the routine LBAPP.
!
!        CALL LBDES ( PTRS, MAXLN, NLINE, NCOM, PCARD )
!
!        IF ( NLINE .LT. MAXLN ) THEN
!           CALL LBAPP ( LINE, PTRS, BUFFER )
!
!        ELSE
!           WRITE (6,*) 'Sorry, there isn't room for another line.'
!           WRITE (6,*) 'Please delete something and try again.'
!        END IF
!
!     For more examples, see the source code of the other LB routines.
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
!     Dagny Taggart, (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 19-JAN-1989 (DT)
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDI
      LOGICAL               RETURN
      INTEGER               SIZEI

!
!     Local variables
!
      INTEGER               PSIZE


!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LBDES_1' )
      END IF

!
!     Recover some information directly.
!
      PSIZE = SIZEI ( PTRS     )
      PCARD = CARDI ( PTRS     )
      NLINE =         PTRS(-2)

!
!     Infer the rest.
!
      MAXLN = ( PSIZE / 4 ) - 1
      NCOM  = ( PCARD / 2 ) - NLINE

      CALL CHKOUT ( 'LBDES_1' )
      RETURN
      END
