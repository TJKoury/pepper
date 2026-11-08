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
      SUBROUTINE NSPLG  ( COMMND, HIDDEN, VSTYLE, HSTYLE, CDELIM )
!
! $ Version
!
!-     Command Loop Configured Version 1.1.0, 21-JUN-1999 (WLT)
!
!         Placed RETURN before first entry point.
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!

!
!     Save the contents of the command to a log file and any save
!     file that might be open and active.
!
      IMPLICIT NONE

      CHARACTER*(*)         COMMND
      LOGICAL               HIDDEN
      CHARACTER*(*)         VSTYLE
      CHARACTER*(*)         HSTYLE
      CHARACTER*(*)         CDELIM

      EXTERNAL              NSPWLN


      INCLUDE              'commdpar.inc'

      INTEGER               LNGSIZ
      PARAMETER           ( LNGSIZ = COMSIZ + 1 )

      CHARACTER*(LNGSIZ)    MYSTR
      CHARACTER*(STYSIZ)    SEEN
      CHARACTER*(STYSIZ)    HIDE
      CHARACTER*(1)         DELIM

      SAVE

      DATA   MYSTR / ' ' /
      DATA   SEEN  / 'LEFT 1 RIGHT 78'                       /
      DATA   HIDE  / 'LEADER ;^ LEFT 1 RIGHT 78 HARDSPACE ^' /
      DATA   DELIM / ';' /
      RETURN
!
!     This entry point handles the logging of commands.
!
      ENTRY NSPLOG ( COMMND, HIDDEN )
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!


      MYSTR = COMMND
!
!     Inhibit writing to the screen.
!
      CALL NSPIOH ( 'SCREEN' )
      CALL NSPIOA ( 'LOG'    )

      IF ( HIDDEN ) THEN
         CALL NICEPR_1   ( COMMND,  HIDE, NSPWLN )
      ELSE
         MYSTR = COMMND
         CALL    SUFFIX   ( DELIM,  0,    MYSTR  )
         CALL NICEPR_1    ( MYSTR,  SEEN, NSPWLN )
      END IF
!
!     Re-activate the screen for writing output.
!
      CALL NSPIOA ( 'SCREEN' )
      CALL NSPIOH ( 'LOG'    )

      RETURN

!
!     This entry point allows users to set the style used for
!     logging hidden and visible commands.
!
      ENTRY NSPLGS ( VSTYLE, HSTYLE, CDELIM )
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!


      SEEN  = VSTYLE
      HIDE  = HSTYLE
      DELIM = CDELIM

      RETURN

!
!     This entry point allows users to get the style used for
!     logging hidden and visible commands.
!
      ENTRY NSPGLS ( VSTYLE, HSTYLE, CDELIM )

      VSTYLE = SEEN
      HSTYLE = HIDE
      CDELIM = DELIM
      RETURN


      END
