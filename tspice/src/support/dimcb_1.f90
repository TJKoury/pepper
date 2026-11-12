
!$Procedure DIMCB ( Dimension of character buffer )

      INTEGER FUNCTION DIMCB_1 ( BUFFER )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Return the dimension of a character buffer.
!
! $ Required_Reading
!
!     CB
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     STRING
!     TEXT
!
! $ Declarations

      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF = 0 )

      CHARACTER*(*)         BUFFER      ( LBCBUF:* )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BUFFER     I   Character buffer.
!
! $ Detailed_Input
!
!     BUFFER      is a character buffer.
!
! $ Detailed_Output
!
!     The function returns the dimension of the character buffer
!     (as established by a previous call to CBINIT).
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
!     The dimension of a character buffer is multiplied by the
!     length of the individual elements in the buffer array to
!     give the total size of the buffer.
!
! $ Examples
!
!     The code fragment
!
!        INTEGER            LBCBUF
!        PARAMETER        ( LBCBUF = 0 )
!
!        INTEGER            DIMCB
!        INTEGER            SIZECB
!        CHARACTER*100      BUFFER  ( LBCBUF:200 )
!
!        CALL CBINIT ( 200, BUFFER )
!
!        WRITE (*,*) DIMCB  ( BUFFER ),    ' elements at '
!        WRITE (*,*) LEN    ( BUFFER(1) ), ' characters each totals '
!        WRITE (*,*) SIZECB ( BUFFER ),    ' characters of storage.'
!
!     produces the following output.
!
!        200    elements at
!        100    characters each totals
!        200000 characters of storage.
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
!     Beta Version 1.1.0, 28-Dec-1994 (WLT)
!
!        Gave DIMCB_1 an initial value of zero so that the function
!        will have a value when it returns even if an error is
!        signalled.
!
!-    Beta Version 1.0.0, 19-JAN-1989 (DT)
!
!-&

!
!     SPICELIB functions
!

!
!     Local variables
!
      INTEGER               DIM
!
!     Give the function an initial value even if it is bogus in the
!     event that we are in RETURN mode.
!
      DIMCB_1 = 0

!
!     Standard error handling.
!


      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DIMCB_1' )
      END IF

!
!     Only the first eight bytes are used.
!
      CALL DECHAR ( BUFFER(0)(1:8), DIM )
      DIMCB_1 = DIM

      CALL CHKOUT ( 'DIMCB_1' )
      RETURN
      END
