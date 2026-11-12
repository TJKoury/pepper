      SUBROUTINE NSPOPL ( LOGNAM, VERSN )
      use pepper_legacy

!
! $ Version
!
!-     Command Loop Configured Version 2.0.0, 10-SEP-1998 (WLT)
!
!         The routine now logs the version of SPICELIB that the
!         program was linked against.
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!

      IMPLICIT NONE
      CHARACTER*(*)         LOGNAM
      CHARACTER*(*)         VERSN
!
!     This routine opens the log file that will be used for loging
!     commands.  It should only be called once.  If a log file
!     cannot be opened, the routine will issue a warning message
!     to the default output device.
!
      LOGICAL               HAVE

      INTEGER               LNGSIZ
      PARAMETER           ( LNGSIZ = 800 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE  = 80 )

      INTEGER               ROOM
      PARAMETER           ( ROOM   = 2 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INCLUDE              'commdpar.inc'

      CHARACTER*(FILEN)     LOGFIL
      CHARACTER*(LNGSIZ)    MYERR ( 2 )
      CHARACTER*(LNGSIZ)    REST
      CHARACTER*(LNSIZE)    STYLE
      CHARACTER*(LNSIZE)    ENV
      CHARACTER*(LNSIZE)    TKV
      CHARACTER*(LNSIZE)    ERR

      CHARACTER*(WDSIZE)    ATTR ( ROOM )
      CHARACTER*(WDSIZE)    WAS
      CHARACTER*(WDSIZE)    VALUE
      CHARACTER*(WDSIZE)    IO
      CHARACTER*(WDSIZE)    TIME
      CHARACTER*(WDSIZE)    WARN

      INTEGER               I
      INTEGER               N
      INTEGER               PTR
      INTEGER               START
      LOGICAL               FOUND


      EXTERNAL NSPWLN
!
!     Empty out the internal error buffers.
!

      MYERR(1) = ' '
      MYERR(2) = ' '

      DO I = 1, ROOM
         ATTR(I) = ' '
      END DO

      CALL NEWFIL ( LOGNAM, 'LOG', LOGFIL )

      IF ( HAVE(MYERR) ) THEN
!
!        See if we can parse the error message as having the
!        string IOSTAT was value imbedded in it.  This isn't
!        pretty, but we can possibly get a better idea of
!        what went wrong this way.
!
         START = POS ( MYERR(1), 'IOSTAT', 1 )

         IF ( START .GT. 0 ) THEN

            REST = MYERR(1)(START:)

            CALL NEXTWD ( REST, IO,    REST )
            CALL NEXTWD ( REST, WAS,   REST )
            CALL NEXTWD ( REST, VALUE, REST )

            IF (      ( EQSTR ( WAS, 'was' ) ) &
                .AND. ( VALUE .NE.   ' '     ) ) THEN

               ERR = ' '
               CALL NPARSI ( VALUE, I, ERR, PTR )

               IF ( ERR .EQ. ' ' ) THEN

                  CALL DCYPHR ( I, FOUND, REST )

                  IF ( FOUND ) THEN
                     MYERR(1)(START:) = REST
                  END IF

               END IF

            END IF
         END IF

         REST = MYERR(1)
         WARN = ' '

         CALL TRNLAT ( 'WARNING',       WARN     )
         CALL TRNLAT ( 'CANNOTOPENLOG', MYERR(2) )

         START = RTRIM ( MYERR(2) )

         CALL PREFIX ( MYERR(2)(1:START), 1, REST )

         STYLE = 'LEFT 1 RIGHT 78 NEWLINE /cr FLAG ' // WARN

         CALL NICEPR_1 ( REST,  STYLE, NSPWLN )
      ELSE
         CALL CURTIM ( TIME          )
         CALL PLTFRM ( ROOM, N, ATTR )
         CALL TKVRSN ( 'TOOLKIT', TKV )
         ENV = ATTR(1)
         CALL SUFFIX ( '---',            1, ENV )
         CALL SUFFIX ( ATTR(2),          1, ENV )
         CALL PREFIX ( 'SPICE Toolkit ', 1, TKV )

         CALL NSPLOG ( ENV,   .TRUE. )
         CALL NSPLOG ( VERSN, .TRUE. )
         CALL NSPLOG ( TKV,   .TRUE. )
         CALL NSPLOG ( TIME,  .TRUE. )
      END IF

      RETURN
      END
