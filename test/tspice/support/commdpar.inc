! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!     The following parameters are the system parameters required
!     by PERCY.  Be sure to read any notes before adjusting these
!
!
!     The maximum number of commands that can be buffered is
!     determined by the value of MAXCOM.  This parameter is
!     used primarily by NXTCOM.
!
      INTEGER               MAXCOM
      PARAMETER           ( MAXCOM = 20 )


!
!     The parameter FILEN is the maximum length of a file name
!     on a particular system.
!
      INTEGER               FILEN
      PARAMETER           ( FILEN = 128 )

!
!     The parameter COMSIZ is the maximum length allowed for a
!     command.
!
      INTEGER               COMSIZ
      PARAMETER           ( COMSIZ = 1024 )

!
!     The parameter ERRSIZ is the maximum length allowed for
!     error messages.
!
      INTEGER               ERRSIZ
      PARAMETER           ( ERRSIZ = 1760 )
!
!     The parameter STYSIZ is the maximum length expected for
!     a NICEPR style string.
!
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )
