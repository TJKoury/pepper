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
      SUBROUTINE NSPOPL ( LOGNAM, VERSN )

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
      INTEGER               POS
      INTEGER               RTRIM
      LOGICAL               EQSTR

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


      EXTERNAL               NSPWLN
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
