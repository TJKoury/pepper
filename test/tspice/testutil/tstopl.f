C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
      SUBROUTINE TSTOPL ( LOGNAM, VERSN )
 
C
C$ Version
C
C-     Test Utilities Version 1.1.0, 12-MAY-1998 (WLT)
C
C         Added output of the toolkit version to the logfile.
C
C-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 4, 1994
C
 
      IMPLICIT NONE
      CHARACTER*(*)         LOGNAM
      CHARACTER*(*)         VERSN
C
C     This routine opens the log file that will be used for loging
C     commands.  It should only be called once.  If a log file
C     cannot be opened, the routine stop.
C
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
 
 
C
C     Empty out the internal error buffers.
C
 
      CALL TSTFIL ( LOGNAM, 'LOG', LOGFIL )
 
      IF ( FAILED() ) THEN
         WRITE (*,*)
         WRITE (*,*) 'A log file cannot be opened.'
         WRITE (*,*)
         STOP
      END IF
C
C     Fetch the current time and the current platform information.
C
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
C
C     Save the Environment, Version and Time so that it may be used
C     in an error or pass log.
C
      CALL TSTSAV ( ENV, VERSN, TIME )
 
      RETURN
 
 
      END
 
 
 
 
 
 
 
 
 
 
