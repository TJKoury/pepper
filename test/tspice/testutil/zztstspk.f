C$Procedure ZZTSTSPK ( Create an SPK file for use in testing )

      SUBROUTINE ZZTSTSPK ( FILE )

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
C     None.
C
C$ Keywords
C
C     SPK
C     TESTING
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         FILE

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FILE       I   The name of an SPK file to create.
C
C$ Detailed_Input
C
C     FILE     is the name of an SPK file to create for use in
C              software testing. This SPK is not a good model
C              for the solar system.
C
C              If the file specified already exists, the existing
C              file is deleted and a new one created with the
C              same name in its place.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the specified file already exists, it is deleted and
C         replaced by the file created by this routine.
C
C     2)  If any error is detected during the creation of the specified
C         SPKf file, the error is signaled by a routine in the call tree
C         of this routine.
C
C$ Files
C
C     This routine creates an SPK file with ephemeris information
C     for the following objects.
C
C          SUN
C             MERCURY
C                MERCURY_BARYCENTER
C             VENUS_BARYCENTER
C                VENUS
C             EARTH-MOON-BARYCENTER
C                EARTH
C                   GOLDSTONE_TRACKING_STATION
C                   MADRID_TRACKING_STATION
C                   CANBERRA_TRACKING_STATION
C                   MOON
C                      SPACECRAFT_PHOENIX
C                      TRANQUILITY_BASE
C             MARS_BARYCENTER
C                MARS
C                   PHOBOS
C                      PHOBOS_BASECAMP
C                   DEIMOS
C             JUPITER_BARYCENTER
C                JUPITER
C                   IO
C                   EUROPA
C                   GANYMEDE
C                   CALLISTO
C             SATURN_BARYCENTER
C                SATURN
C
C                   TITAN
C             URANUS_BARYCENTER
C                URANUS
C                   OBERON
C                   ARIEL
C                   UMBRIEL
C                   TITANIA
C                   MIRANDA
C             NEPTUNE_BARYCENTER
C                NEPTUNE
C                   TRITON
C                   NEREID
C             PLUTO_BARYCENTER
C                PLUTO
C                   CHARON
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
C     CALL ZZTSTSPK ( 'sstoy.bsp' )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    Test Utilities Version 1.1.0, 30-MAY-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
C
C-    Test Utilities Version 1.0.1, 05-NOV-2007 (EDW)
C
C        Minor header edits. No affect on functionality.
C
C-    Test Utilities Version 1.0.0, 07-JAN-2006 (EDW)
C
C        Reimplementation of TSTSPK removing all code
C        except that needed to write the test SPK.
C
C-&


C$ Index_Entries
C
C     Create an SPK file for high-level software tests
C
C-&


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
      INTEGER               HANDLE
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
      CALL SPCOPN ( FILE, 'TestUtilitySPK', HANDLE )
C
C     Now just construct the state information needed  to create
C     segments for all of the various objects that we are going
C     to simulate.
C
      EPOCH       =  -189345600.0D0

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

            CALL SPKW05 ( HANDLE, BODY,  CENTER,
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

            CALL SPKW08 (HANDLE, BODY,   CENTER,
     .                   FRAME,  FIRST,  LAST,
     .                   SEGID,  DEGREE, N,
     .                   STATES, FIRST,  STEP   )

         END IF

         I = I + 1
         CALL TSTSTC ( I, BODY )

      END DO

      CALL DAFCLS ( HANDLE )


      RETURN
      END
