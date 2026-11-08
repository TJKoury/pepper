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
      SUBROUTINE TSTOPL ( LOGNAM, VERSN )

!
! $ Version
!
!-     Test Utilities Version 1.1.0, 12-MAY-1998 (WLT)
!
!         Added output of the toolkit version to the logfile.
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
!     cannot be opened, the routine stop.
!
      LOGICAL               FAILED

      INTEGER               FILEN
      PARAMETER           ( FILEN = 255 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE  = 80 )

      INTEGER               ROOM
      PARAMETER           ( ROOM   = 2 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )


      CHARACTER*(FILEN)     LOGFIL
      CHARACTER*(LNSIZE)    ENV  ( 2    )

      CHARACTER*(WDSIZE)    ATTR ( ROOM )
      CHARACTER*(WDSIZE)    TIME
      CHARACTER*(WDSIZE)    TKV

      INTEGER               N
      SAVE


!
!     Empty out the internal error buffers.
!

      CALL TSTFIL ( LOGNAM, 'LOG', LOGFIL )

      IF ( FAILED() ) THEN
         WRITE (*,*)
         WRITE (*,*) 'A log file cannot be opened.'
         WRITE (*,*)
         STOP
      END IF
!
!     Fetch the current time and the current platform information.
!
      CALL CURTIM ( TIME              )
      CALL PLTFRM ( ROOM,   N,  ATTR  )
      CALL TKVRSN ( 'TOOLKIT',  TKV   )

      ENV(1) = '--'
      ENV(2) = '--'
      CALL SUFFIX ( ATTR(1), 1, ENV(1)   )
      CALL SUFFIX ( '--',    1, ENV(1)   )
      CALL SUFFIX ( ATTR(2), 1, ENV(2)   )
      CALL SUFFIX ( '--',    1, ENV(2)   )

      CALL TSTLOG ( ENV(1),    .FALSE. )
      CALL TSTLOG ( ENV(2),    .FALSE. )
      CALL TSTLOG ( VERSN,     .FALSE. )
      CALL TSTLOG ( TKV,       .FALSE. )
      CALL TSTLOG ( TIME,      .FALSE. )
!
!     Save the Environment, Version and Time so that it may be used
!     in an error or pass log.
!
      CALL TSTSAV ( ENV, VERSN, TIME )

      RETURN


      END










