C$Procedure      TSTSPK ( Create an SPK file for use in testing)
 
      SUBROUTINE TSTSPK ( FILE, LOAD, HANDLE )
 
C$ Abstract
C
C     Create an SPK file that can be used for obtaining
C     states and testing code that makes use of the SPK system
C
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
C$ Required_Reading
C
C      None.
C
C$ Keywords
C
C       TESTING
C       SPK
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         FILE
      LOGICAL               LOAD
      INTEGER               HANDLE
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      FILE       I   The name of an SPK file to create.
C      LOAD       I   Logical indicating if file should be loaded.
C      HANDLE     O   Handle if file is loaded by TSTSPK.
C
C$ Detailed_Input
C
C     FILE        is the name of an SPK file to create for use in
C                 software testing.  This SPK is not a good model
C                 for the solar system.
C
C                 If the file specified already exists, the existing
C                 file is deleted and a new one created with the
C                 same name in its place.
C
C     LOAD        is a logical flag indicating whether or not the
C                 created SPK file should be loaded.  If LOAD is TRUE
C                 the file is loaded.  If LOAD is FALSE the file is
C                 not loaded by this routine.
C
C
C$ Detailed_Output
C
C     HANDLE      is the handle attached to the SPK file if LOAD is
C                 true.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     1) If the specified file already exists, it is deleted and
C        replaced by the file created by this routine.
C
C     1) All other exceptions are diagnosed by routines in the call tree
C        of this routine.
C
C$ Files
C
C      This routine creates an SPK file with ephemeris information
C      for the following objects.
C
C           SUN
C              MERCURY
C                 MERCURY_BARYCENTER
C              VENUS_BARYCENTER
C                 VENUS
C              EARTH-MOON-BARYCENTER
C                 EARTH
C                    GOLDSTONE_TRACKING_STATION
C                    MADRID_TRACKING_STATION
C                    CANBERRA_TRACKING_STATION
C                    MOON
C                       SPACECRAFT_PHOENIX
C                       TRANQUILITY_BASE
C              MARS_BARYCENTER
C                 MARS
C                    PHOBOS
C                       PHOBOS_BASECAMP
C                    DEIMOS
C              JUPITER_BARYCENTER
C                 JUPITER
C                    IO
C                    EUROPA
C                    GANYMEDE
C                    CALLISTO
C              SATURN_BARYCENTER
C                 SATURN
C
C                    TITAN
C              URANUS_BARYCENTER
C                 URANUS
C                    OBERON
C                    ARIEL
C                    UMBRIEL
C                    TITANIA
C                    MIRANDA
C              NEPTUNE_BARYCENTER
C                 NEPTUNE
C                    TRITON
C                    NEREID
C              PLUTO_BARYCENTER
C                 PLUTO
C                    CHARON
C
C
C$ Particulars
C
C     This routine creates a "TOY" solar system model for use
C     in testing the SPICE ephemeris system.
C
C     The data in this file is "good" for the epochs
C
C        from 1980 JAN 1, 00:00:00.000 (ET)
C        to   2011 SEP 9, 01:46:40.000 (ET)
C        (a span of exactly 1 billion seconds).
C
C
C     If the input file already exists, it is deleted prior to the
C     creation of this file.
C
C$ Examples
C
C     The normal way to use this routine is shown below.
C
C     CALL TSTSPK ( 'sstoy.bsp', .TRUE., HANDLE  )
C
C        perform some tests and computations.
C
C
C     CALL SPKUEF ( HANDLE )
C     CALL KILFIL ( 'sstoy.bsp' )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Version
C
C-    Test Utilities 1.2.0, 31-JAN-2014 (BVS)
C
C        BUG FIX: added "D0" in EPOCH assignment.
C
C-    Test Utilities 1.1.0, 28-JUL-1999 (WLT)
C
C        Added code so that the SPK file will be registered with
C        the Test Utilities File Registry.  This allows it to
C        be deleted automatically when a test family is finished.
C
C-    Test Utilities 1.0.0, 6-APR-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Create an SPK file for high-level software tests
C
C-&
C
C     Spicelib Functions
C
      INTEGER               RTRIM
 
C
C     Local Variables.
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
 
      CHARACTER*(WDSIZE)    FRAME
      CHARACTER*(WDSIZE)    SEGID
 
      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               DEGREE
      INTEGER               I
      INTEGER               MYHAND
      INTEGER               N
      INTEGER               REF
 
 
      DOUBLE PRECISION      GM
      DOUBLE PRECISION      STATES(6,2)
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      STEP
 
C
C     Wipe out any existing file with the target name.  Then open
C     a new SPC file for writing.
C
      CALL KILFIL ( FILE )
      CALL SPCOPN ( FILE, 'TestUtilitySPK', MYHAND )
C
C     Now just construct the state information needed  to create
C     segments for all of the various objects that we are going
C     to simulate.
C
      EPOCH       =  -189345600.000000D0
 
      I           =   1
      CALL TSTSTC ( I, BODY )
 
      DO WHILE ( BODY .NE. 0 )
 
         CALL TSTST  ( BODY,  EPOCH,
     .                 SEGID, REF, STATES, CENTER, GM )
         CALL FRMNAM ( REF,   FRAME                   )
 
         IF ( GM .GT. 0.0D0 ) THEN
 
            FIRST       = -5.0D08
            LAST        =  5.0D08
            N           =  1
 
            CALL SPKW05 ( MYHAND, BODY,  CENTER,
     .                     FRAME,  FIRST, LAST,
     .                     SEGID,  GM,    N,
     .                     STATES, EPOCH       )
 
         ELSE
 
            FIRST       = -5.0D08
            LAST        =  5.0D08
            N           =  2
            STEP        =  1.0D09
            DEGREE      =  1
 
            STATES(1,2) =  STATES(1,1)
            STATES(2,2) =  STATES(2,1)
            STATES(3,2) =  STATES(3,1)
            STATES(4,2) =  STATES(4,1)
            STATES(5,2) =  STATES(5,1)
            STATES(6,2) =  STATES(6,1)
 
            CALL SPKW08 (MYHAND, BODY,   CENTER,
     .                   FRAME,  FIRST,  LAST,
     .                   SEGID,  DEGREE, N,
     .                   STATES, FIRST,  STEP   )
 
         END IF
 
         I = I + 1
         CALL TSTSTC ( I, BODY )
 
      END DO
 
      CALL DAFCLS ( MYHAND )
 
C
C     If the user wants this file loaded, now is the time to do it.
C
      IF ( LOAD ) THEN
 
         CALL SPKLEF( FILE(1:RTRIM(FILE)), HANDLE )
 
      END IF
C
C     Register this file with FILREG so it will automatically be
C     removed when a new test family is initialized.
C
      CALL TFILES ( FILE )
 
 
      RETURN
 
 
      END
