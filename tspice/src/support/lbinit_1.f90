
!$Procedure LBINIT ( Line buffer, initialize )

      SUBROUTINE LBINIT_1 ( PSIZE, VDIM, PTRS, BUFFER )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Initialize a line buffer.
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

      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF =  0 )

      INTEGER               PSIZE
      INTEGER               VDIM
      INTEGER               PTRS        ( LBCELL:PSIZE )
      CHARACTER*(*)         BUFFER      ( LBCBUF:VDIM  )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     PSIZE      I   Pointer size.
!     VDIM       I   Value dimension.
!     PTRS,
!     BUFFER    I,O  Line buffer.
!
! $ Detailed_Input
!
!     PTRS        is an integer cell array to be used as the pointer
!                 component of a line buffer.
!
!     PSIZE       is the declared dimension of PTRS.
!
!     BUFFER      is a character buffer array to be used as the
!                 character compnent of a line buffer.
!
!     VDIM        is the declared dimension of BUFFER.
!
! $ Detailed_Output
!
!     PTRS,
!     BUFFER      together are an initialized line buffer.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If the line buffer cannot hold even a single line, the error
!        'SPICE(LBINSUFPTRSIZE)' is signalled.
!
! $ Particulars
!
!     A line buffer must be initialized to allow subsequent
!     operations on the buffer to detect possible overflows.
!     Both components of the buffer are initialized by a single
!     call to LBINIT.
!
!     In order to store N lines, PSIZE should be at least 4N+4.
!
! $ Examples
!
!     The following code fragment illustrates the initialization
!     of a typical line buffer.
!
!        INTEGER               LBCELL
!        PARAMETER           ( LBCELL =    -5 )
!
!        INTEGER               LBCBUF
!        PARAMETER           ( LBCBUF =     0 )
!
!        INTEGER               MAXLN
!        PARAMETER           ( MAXLN  =  1000 )
!
!        INTEGER               PSIZE
!        PARAMETER           ( PSIZE  =  4 * MAXLN + 4 )
!
!        INTEGER               BUFDIM
!        PARAMETER           ( BUFDIM =    25 )
!
!        INTEGER               PTRS     ( LBCELL:PSIZE   )
!        CHARACTER*(MAXLN)     BUFFER   ( LBCBUF:BUFDIM  )
!         .
!         .
!
!        CALL LBINIT ( PSIZE, BUFDIM, PTRS, BUFFER )
!
!     In this example, the buffer may be used to store up to 1000 lines
!     averaging 25 characters per line, or 25,000 total characters. The
!     length of any particular line may range from a single character
!     to the entire 25,000 characters.
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
      INTEGER               SIZECB_1

!
!     Local variables
!
      INTEGER               MAXLN


!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LBINIT_1' )
      END IF

!
!     Initialize the character buffer first.
!
      CALL CBINIT_1 ( VDIM, BUFFER )

!
!     The size must be 4(n+1), where n is the maximum number of
!     lines that can be stored. (The line buffer must be able to
!     store at least one line!)
!
!     Every line buffer starts out with zero lines and one complement
!     interval, which covers the entire CB.
!
      MAXLN = ( PSIZE / 4 ) - 1

      IF ( MAXLN .LT. 1 ) THEN
         CALL SIGERR ( 'SPICE(INSUFPTRSIZE)' )

      ELSE
         CALL SSIZEI ( 4 * (MAXLN + 1), PTRS )

         PTRS(1) = 1
         PTRS(2) = SIZECB_1 ( BUFFER )

         CALL LBUPD_1 ( 0, 1, PTRS )
      END IF

      CALL CHKOUT ( 'LBINIT_1' )
      RETURN
      END
