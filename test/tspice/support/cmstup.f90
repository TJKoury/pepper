!$Procedure      CMSTUP ( Command Loop Startup )

      SUBROUTINE CMSTUP

! $ Abstract
!
!     This routine performs command loop start ups associated
!     with information on the command line when the user
!     activated the program.
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
!      None.
!
! $ Keywords
!
!       Command Loop
!
! $ Declarations

      IMPLICIT NONE


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!      None.
!
! $ Files
!
!      None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This routine examines the information supplied on the command
!     line when a program was started and sets the symbols indicating
!     whether or not the program is in batch mode and if appropriate
!     sets up to start a command procedure.
!
!     This routine works entirely by side effect.
!
!     Recognized flags are:
!
!     -b                for batch mode
!     -start filename   for starting a startup file.
!
!     Unrecognized options are ignored.
!
! $ Examples
!
!     See the command loop documentation
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 20-NOV-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Command loop set up.
!
!-&

!
!     Command loop fucntions
!
      LOGICAL               SETBAT

!
!     Below are the various sources from which
!     commands might come.
!
!     NONE
!     COMBUF
!     KEYBRD
!     INPFIL
!
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

      INTEGER               COMBUF
      PARAMETER           ( COMBUF = NONE   + 1 )

      INTEGER               KEYBRD
      PARAMETER           ( KEYBRD = COMBUF + 1 )

      INTEGER               INPFIL
      PARAMETER           ( INPFIL = KEYBRD + 1 )



      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      CHARACTER*(LNSIZE)    COMMND
      CHARACTER*(LNSIZE)    COMLIN
      CHARACTER*(LNSIZE)    FILE

      INTEGER               B
      INTEGER               E
      INTEGER               START

      LOGICAL               HAVGO
      LOGICAL               DOBTCH
      LOGICAL               HAVFIL

      CALL GETCML ( COMLIN )

      START  = 1
      HAVGO  = .FALSE.
      DOBTCH = .FALSE.
      HAVFIL = .FALSE.


      CALL FNDNWD ( COMLIN, START, B, E )

      DO WHILE ( B .GT. 0 )

         IF ( COMLIN(B:E) .EQ. '-b' ) THEN

            DOBTCH = .TRUE.

         ELSE IF ( COMLIN(B:E) .EQ. '-start' ) THEN

            HAVGO = .TRUE.

         ELSE IF ( HAVGO .AND. .NOT. HAVFIL ) THEN

            FILE   = COMLIN(B:E)
            HAVFIL = .TRUE.

         END IF

         START = E + 1
         CALL FNDNWD ( COMLIN, START, B, E )
      END DO

!
!     If we have a batch flag, notify NXTCOM
!
      IF ( DOBTCH ) THEN

         DOBTCH = SETBAT()

      END IF

      IF ( HAVGO .AND. HAVFIL ) THEN

         CALL TRNLAT ( 'START',  COMMND )
         CALL SUFFIX (  FILE, 1, COMMND )
         CALL PUTCOM (  COMMND,  COMBUF )

      END IF


      RETURN
      END
