
!$Procedure SBINIT ( String buffer, initialize )

      SUBROUTINE SBINIT_1 ( NSIZE, PSIZE, VDIM, NAMES, &
                                              PTRS, &
                                              BUFFER )
      IMPLICIT NONE


! $ Abstract
!
!
! $ Required_Reading
!
!     CB, LB, SB
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

      INTEGER               NSIZE
      INTEGER               PSIZE
      INTEGER               VDIM
      CHARACTER*(*)         NAMES       ( LBCELL:NSIZE )
      INTEGER               PTRS        ( LBCELL:PSIZE )
      CHARACTER*(*)         BUFFER      ( LBCBUF:VDIM  )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NSIZE      I   Name size.
!     PSIZE      I   Pointer size.
!     VDIM       I   Value dimension
!     NAMES,
!     PTRS,
!     BUFFER    I,O  String buffer.
!
! $ Detailed_Input
!
!     NAMES       is a character cell array to be used as the name
!                 component of a string buffer.
!
!     NSIZE       is the declared dimension of NAMES.
!
!     PTRS        is an integer cell array to be used as the pointer
!                 component of a string buffer.
!
!     PSIZE       is the declared dimension of PTRS.
!
!     BUFFER      is a character buffer array to be used as the
!                 character component of a string buffer.
!
!     VDIM        is the declared dimension of BUFFER.
!
! $ Detailed_Output
!
!     NAMES,
!     PTRS,
!     BUFFER      together are an initialized string buffer.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If the size of the pointer array is not sufficient to
!        hold pointers for the maximum number of strings, the
!        error 'SPICE(SBINSUFPTRSIZE)' is signalled.
!
! $ Particulars
!
!     A string buffer must be initialized to allow subsequent
!     operations on the buffer to detect possible overflows.
!     All three components of the buffer are initialized by a
!     single call to SBINIT.
!
!     In order to make full use of the name cell of the string buffer,
!     the arrays and name buffers should be declared as shown below.
!
!       INTEGER              LBCELL
!       PARAMETER          ( LBCELL = -5 )
!
!       INTEGER              MAXNL
!       PARAMETER          ( MAXNL  = maximum name length )
!
!       INTEGER              MAXN
!       PARAMETER          ( MAXN   = maximum number of names )
!
!       CHARACTER*(MAXNL)    NAMES ( MAXN         )
!       INTEGER              PTRS  ( MAXN * 4 + 4 )
!
!     The character buffer portion of the string buffer should be
!     declared as shown below.
!
!       INTEGER              MAXL
!       PARAMETER          ( MAXL = maximum expected string length )
!
!       INTEGER              AVGL
!       PARAMETER          ( AVGL = average expected string length )
!
!       INTEGER              LBCBUF
!       PARAMETER          ( LBCBUF = 0  )
!
!       CHARACTER*(MAXL)     BUFFER ( LBCBUF:(MAXN * AVGL) / MAXL + 1 )
!
! $ Examples
!
!     The following code fragment illustrates the initialization
!     of a typical string buffer.
!
!        INTEGER               LBCELL
!        PARAMETER           ( LBCELL =    -5 )
!
!        INTEGER               LBCBUF
!        PARAMETER           ( LBCBUF =     0 )
!
!        CHARACTER*(32)        NAMES    ( LBCELL:1000    )
!        INTEGER               PTRS     ( LBCELL:4004    )
!        CHARACTER*(250)       BUFFER   ( LBCBUF:100     )
!         .
!         .
!
!        CALL SBINIT ( MAXN, PSIZE, BUFDIM, NAMES, PTRS, BUFFER )
!
!     In this example, the buffer may be used to store up to 1000
!     strings averaging 25 characters per string, or 25,000 total
!     characters. The length of any particular string may range from
!     a single character to the entire 25,000 characters. The names
!     used to identify the strings may contain up to 32 characters.
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
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               MAXPTR


!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SBINIT_1' )
      END IF

!
!     Make sure that the line buffer is large enough (but ONLY large
!     enough) to hold the maximum number of strings. The name list
!     should be empty. The LB should be initialized as a unit.
!
      MAXPTR = 4 * ( NSIZE + 1 )

      IF ( PSIZE .LT. MAXPTR ) THEN
         CALL SIGERR ( 'SPICE(SBINSUFPTRSIZE)' )

      ELSE
         CALL SSIZEC ( NSIZE,        NAMES         )
         CALL LBINIT_1 ( MAXPTR, VDIM, PTRS, BUFFER )
      END IF

      CALL CHKOUT ( 'SBINIT_1' )
      RETURN
      END
