!$Procedure ZZTSTSPK ( Create an SPK file for use in testing )

      SUBROUTINE ZZTSTSPK ( FILE )

! $ Abstract
!
!     Create an SPK file that can be used for obtaining
!     states and testing code that makes use of the SPK system
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
!     None.
!
! $ Keywords
!
!     SPK
!     TESTING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         FILE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILE       I   The name of an SPK file to create.
!
! $ Detailed_Input
!
!     FILE     is the name of an SPK file to create for use in
!              software testing. This SPK is not a good model
!              for the solar system.
!
!              If the file specified already exists, the existing
!              file is deleted and a new one created with the
!              same name in its place.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the specified file already exists, it is deleted and
!         replaced by the file created by this routine.
!
!     2)  If any error is detected during the creation of the specified
!         SPKf file, the error is signaled by a routine in the call tree
!         of this routine.
!
! $ Files
!
!     This routine creates an SPK file with ephemeris information
!     for the following objects.
!
!          SUN
!             MERCURY
!                MERCURY_BARYCENTER
!             VENUS_BARYCENTER
!                VENUS
!             EARTH-MOON-BARYCENTER
!                EARTH
!                   GOLDSTONE_TRACKING_STATION
!                   MADRID_TRACKING_STATION
!                   CANBERRA_TRACKING_STATION
!                   MOON
!                      SPACECRAFT_PHOENIX
!                      TRANQUILITY_BASE
!             MARS_BARYCENTER
!                MARS
!                   PHOBOS
!                      PHOBOS_BASECAMP
!                   DEIMOS
!             JUPITER_BARYCENTER
!                JUPITER
!                   IO
!                   EUROPA
!                   GANYMEDE
!                   CALLISTO
!             SATURN_BARYCENTER
!                SATURN
!
!                   TITAN
!             URANUS_BARYCENTER
!                URANUS
!                   OBERON
!                   ARIEL
!                   UMBRIEL
!                   TITANIA
!                   MIRANDA
!             NEPTUNE_BARYCENTER
!                NEPTUNE
!                   TRITON
!                   NEREID
!             PLUTO_BARYCENTER
!                PLUTO
!                   CHARON
!
! $ Particulars
!
!     This routine creates a "TOY" solar system model for use
!     in testing the SPICE ephemeris system.
!
!     The data in this file is "good" for the epochs
!
!        from 1980 JAN 1, 00:00:00.000 (ET)
!        to   2011 SEP 9, 01:46:40.000 (ET)
!        (a span of exactly 1 billion seconds).
!
!
!     If the input file already exists, it is deleted prior to the
!     creation of this file.
!
! $ Examples
!
!     The normal way to use this routine is shown below.
!
!     CALL ZZTSTSPK ( 'sstoy.bsp' )
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
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    Test Utilities Version 1.1.0, 30-MAY-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
!
!-    Test Utilities Version 1.0.1, 05-NOV-2007 (EDW)
!
!        Minor header edits. No affect on functionality.
!
!-    Test Utilities Version 1.0.0, 07-JAN-2006 (EDW)
!
!        Reimplementation of TSTSPK removing all code
!        except that needed to write the test SPK.
!
!-&


! $ Index_Entries
!
!     Create an SPK file for high-level software tests
!
!-&


!
!     Local Variables.
!
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

!
!     Wipe out any existing file with the target name.  Then open
!     a new SPC file for writing.
!
      CALL KILFIL ( FILE )
      CALL SPCOPN ( FILE, 'TestUtilitySPK', HANDLE )
!
!     Now just construct the state information needed  to create
!     segments for all of the various objects that we are going
!     to simulate.
!
      EPOCH       =  -189345600.0D0

      I           =   1
      CALL TSTSTC ( I, BODY )

      DO WHILE ( BODY .NE. 0 )

         CALL TSTST  ( BODY,  EPOCH, &
                       SEGID, REF, STATES, CENTER, GM )
         CALL FRMNAM ( REF,   FRAME                   )

         IF ( GM .GT. 0.0D0 ) THEN

            FIRST       = -5.0D08
            LAST        =  5.0D08
            N           =  1

            CALL SPKW05 ( HANDLE, BODY,  CENTER, &
                           FRAME,  FIRST, LAST, &
                           SEGID,  GM,    N, &
                           STATES, EPOCH       )

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

            CALL SPKW08 (HANDLE, BODY,   CENTER, &
                         FRAME,  FIRST,  LAST, &
                         SEGID,  DEGREE, N, &
                         STATES, FIRST,  STEP   )

         END IF

         I = I + 1
         CALL TSTSTC ( I, BODY )

      END DO

      CALL DAFCLS ( HANDLE )


      RETURN
      END
