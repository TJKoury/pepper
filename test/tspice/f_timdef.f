C$Procedure      F_TIMDEF ( Family of tests for TIMDEF )
 
      SUBROUTINE F_TIMDEF ( OK )
 
C$ Abstract
C
C     This routine exercises the routine TIMDEF to make sure
C     that all advertised inputs are accepted and that
C     unadvertised inputs are rejected.
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
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the higher-level SPICELIB
C     time system routines.
C
C$ Examples
C
C     None.
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
C     B.V. Semenov     (JPL)
C     W.L. Taber       (JPL)
C     E.D. Wright      (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 05-NOV-2020 (EDW)
C
C        Add test case for time system name 'TT' (Terrestrial Time).
C        This time system operates as alternate assignjment of 'TDT' 
C
C-    TSPICE Version 1.0.0, 13-NOV-1996 (WLT)
C
C        A long time ago, probably before 13-NOV-1996 (WLT)
C
C-&
 
C
C     Local Variables
C
      INTEGER               SMWDSZ
      PARAMETER           ( SMWDSZ = 16 )
 
 
      CHARACTER*(SMWDSZ)    VALUE
      CHARACTER*(SMWDSZ)    ZONE
      CHARACTER*(SMWDSZ)    CALNDR
      CHARACTER*(SMWDSZ)    SYSTEM
      CHARACTER*(SMWDSZ)    ZONES ( 8 )
      CHARACTER*(SMWDSZ)    EXPECT( 8 )
 
      INTEGER               I
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_TIMDEF' )
 
      CALL TCASE ( 'Check that the default values are SYSTEM = '
     .//           '''UTC'', ZONE = '' '' and CALENDAR = '
     .//           '''GREGORIAN'' ' )
 
         CALL TIMDEF ( 'get', 'system', VALUE )
 
         CALL CHCKXC ( .FALSE.,             ' ',   OK )
         CALL CHCKSC ( 'VALUE', VALUE, '=', 'UTC', OK )
 
         CALL TIMDEF ( 'get',  'zone',  VALUE )
 
         CALL CHCKXC ( .FALSE.,             ' ',  OK )
         CALL CHCKSC ( 'VALUE', VALUE, '=', ' ',  OK )
 
         CALL TIMDEF ( 'get',  'calendar',  VALUE )
 
         CALL CHCKXC ( .FALSE.,             ' ',         OK )
         CALL CHCKSC ( 'VALUE', VALUE, '=', 'GREGORIAN', OK )
 
 
 
      CALL TCASE ( 'Make sure we can set and get SYSTEM UTC, '
     .//           'TDT, TDB Make sure Calendar is not affected '
     .//           'and that ZONE returns a blank. ' )
 
C
C        Excessive? 
C
 
         CALL TIMDEF ( 'set', 'system',  'tdb'   )
         CALL TIMDEF ( 'get', 'system',   SYSTEM )
         CALL TIMDEF ( 'get', 'zone',     ZONE   )
         CALL TIMDEF ( 'get', 'calendar', CALNDR )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', 'TDB',       OK )
         CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'GREGORIAN', OK )
         CALL CHCKSC ( 'ZONE',   ZONE,   '=', ' ',         OK )
 
 
         CALL TIMDEF ( 'set', 'system',  'tdt'   )
         CALL TIMDEF ( 'get', 'system',   SYSTEM )
         CALL TIMDEF ( 'get', 'zone',     ZONE   )
         CALL TIMDEF ( 'get', 'calendar', CALNDR )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', 'TDT',       OK )
         CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'GREGORIAN', OK )
         CALL CHCKSC ( 'ZONE',   ZONE,   '=', ' ',         OK )
 

         CALL TIMDEF ( 'set', 'system',  'tt'   )
         CALL TIMDEF ( 'get', 'system',   SYSTEM )
         CALL TIMDEF ( 'get', 'zone',     ZONE   )
         CALL TIMDEF ( 'get', 'calendar', CALNDR )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', 'TT',       OK )
         CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'GREGORIAN', OK )
         CALL CHCKSC ( 'ZONE',   ZONE,   '=', ' ',         OK )
 

         CALL TIMDEF ( 'set', 'system',  'utc'   )
         CALL TIMDEF ( 'get', 'system',   SYSTEM )
         CALL TIMDEF ( 'get', 'zone',     ZONE   )
         CALL TIMDEF ( 'get', 'calendar', CALNDR )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', 'UTC',       OK )
         CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'GREGORIAN', OK )
         CALL CHCKSC ( 'ZONE',   ZONE,   '=', ' ',         OK )
 
 
      CALL TCASE ( 'Make sure we can set and get CALENDAR MIXED, '
     .//           'JULIAN, GREGORIAN Make sure that system and '
     .//           'zone are not affected. ' )
 
         CALL TIMDEF ( 'set', 'calendar',  'Mixed'   )
         CALL TIMDEF ( 'get', 'system',     SYSTEM )
         CALL TIMDEF ( 'get', 'zone',       ZONE   )
         CALL TIMDEF ( 'get', 'calendar',   CALNDR )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', 'UTC',    OK )
         CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'MIXED',  OK )
         CALL CHCKSC ( 'ZONE',   ZONE,   '=', ' ',      OK )
 
 
         CALL TIMDEF ( 'set', 'calendar',  'Julian'   )
         CALL TIMDEF ( 'get', 'system',    SYSTEM )
         CALL TIMDEF ( 'get', 'zone',      ZONE   )
         CALL TIMDEF ( 'get', 'calendar',  CALNDR )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', 'UTC',    OK )
         CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'JULIAN', OK )
         CALL CHCKSC ( 'ZONE',   ZONE,   '=', ' ',      OK )
 
 
         CALL TIMDEF ( 'set', 'calendar',  'Gregorian'   )
         CALL TIMDEF ( 'get', 'system',     SYSTEM )
         CALL TIMDEF ( 'get', 'zone',       ZONE   )
         CALL TIMDEF ( 'get', 'calendar',   CALNDR )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', 'UTC',       OK )
         CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'GREGORIAN', OK )
         CALL CHCKSC ( 'ZONE',   ZONE,   '=', ' ',         OK )
 
      CALL TCASE ( 'Make sure we can set and get all of the U.S. '
     .//           'time zones. Also check that the CALENDAR is '
     .//           'not affected and that SYSTEM returns a '
     .//           'blank. ' )
 
 
 
         ZONES ( 1 ) = 'est'
         ZONES ( 2 ) = 'edt'
         ZONES ( 3 ) = 'cst'
         ZONES ( 4 ) = 'cdt'
         ZONES ( 5 ) = 'mst'
         ZONES ( 6 ) = 'mdt'
         ZONES ( 7 ) = 'pst'
         ZONES ( 8 ) = 'pdt'
 
 
         EXPECT ( 1 ) = 'UTC-5'
         EXPECT ( 2 ) = 'UTC-4'
         EXPECT ( 3 ) = 'UTC-6'
         EXPECT ( 4 ) = 'UTC-5'
         EXPECT ( 5 ) = 'UTC-7'
         EXPECT ( 6 ) = 'UTC-6'
         EXPECT ( 7 ) = 'UTC-8'
         EXPECT ( 8 ) = 'UTC-7'
 
 
         DO I = 1, 8
 
            CALL TIMDEF ( 'set', 'zone',       ZONES(I) )
            CALL TIMDEF ( 'get', 'system',     SYSTEM   )
            CALL TIMDEF ( 'get', 'zone',       ZONE     )
            CALL TIMDEF ( 'get', 'calendar',   CALNDR   )
 
            CALL CHCKXC ( .FALSE., ' ',  OK )
            CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', ' ',         OK )
            CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'GREGORIAN', OK )
            CALL CHCKSC ( 'ZONE',   ZONE,   '=', EXPECT(I),   OK )
 
         END DO
 
      CALL TCASE ( 'Make sure we can set and get several '
     .//           'non-U.S. time zones. ' )
 
 
         CALL TIMDEF ( 'set', 'zone',      'utc+3:19' )
         CALL TIMDEF ( 'get', 'system',     SYSTEM    )
         CALL TIMDEF ( 'get', 'zone',       ZONE      )
         CALL TIMDEF ( 'get', 'calendar',   CALNDR    )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', ' ',           OK )
         CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'GREGORIAN',   OK )
         CALL CHCKSC ( 'ZONE',   ZONE,   '=', 'UTC+3:19',  OK )
 
         CALL TIMDEF ( 'set', 'zone',      'utc-4:27' )
         CALL TIMDEF ( 'get', 'system',     SYSTEM    )
         CALL TIMDEF ( 'get', 'zone',       ZONE      )
         CALL TIMDEF ( 'get', 'calendar',   CALNDR    )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', ' ',           OK )
         CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'GREGORIAN',   OK )
         CALL CHCKSC ( 'ZONE',   ZONE,   '=', 'UTC-4:27',  OK )
 
      CALL TCASE ( 'Make sure that unrecognized ACTIONS trigger '
     .//           'an error. ' )
 
 
         CALL TIMDEF ( 'put', 'zone', 'pdt' )
         CALL CHCKXC ( .TRUE., 'SPICE(BADACTION)',   OK   )
 
      CALL TCASE ( 'Make sure that unrecognized ITEMS trigger an '
     .//           'error. SET case' )
 
         CALL TIMDEF ( 'set', 'zone',      'utc-4:27' )
         CALL TIMDEF ( 'get', 'system',     SYSTEM    )
         CALL TIMDEF ( 'get', 'zone',       ZONE      )
         CALL TIMDEF ( 'get', 'calendar',   CALNDR    )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', ' ',          OK )
         CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'GREGORIAN',  OK )
         CALL CHCKSC ( 'ZONE',   ZONE,   '=', 'UTC-4:27', OK )
 
         CALL TIMDEF ( 'set', 'year', '1950' )
         CALL CHCKXC ( .TRUE., 'SPICE(BADTIMEITEM)',   OK   )
 
 
      CALL TCASE ( 'Make sure that unrecognized ITEMS trigger an '
     .//           'error. GET case' )
 
         CALL TIMDEF ( 'set', 'zone',      'utc-4:27' )
         CALL TIMDEF ( 'get', 'system',     SYSTEM    )
         CALL TIMDEF ( 'get', 'zone',       ZONE      )
         CALL TIMDEF ( 'get', 'calendar',   CALNDR    )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'SYSTEM', SYSTEM, '=', ' ',           OK )
         CALL CHCKSC ( 'CALNDR', CALNDR, '=', 'GREGORIAN',   OK )
         CALL CHCKSC ( 'ZONE',   ZONE,   '=', 'UTC-4:27',  OK )
 
         CALL TIMDEF ( 'GET', 'year', VALUE )
         CALL CHCKXC ( .TRUE., 'SPICE(BADTIMEITEM)',   OK   )
 
 
      CALL TCASE ( 'Make sure that unrecongized time zones '
     .//           'trigger an error. SET case ' )
 
         CALL TIMDEF ( 'set', 'ZONE', 'GMT' )
         CALL CHCKXC ( .TRUE., 'SPICE(BADDEFAULTVALUE)',   OK   )
 
 
      CALL TCASE ( 'Make sure that unrecognized calendars '
     .//           'trigger and error. ' )
 
         CALL TIMDEF ( 'set', 'CALENDAR', 'MUSLIM' )
         CALL CHCKXC ( .TRUE., 'SPICE(BADDEFAULTVALUE)',   OK   )
 
      CALL TCASE ( 'Make sure that unrecognized systems trigger '
     .//           'an error. ' )
 
         CALL TIMDEF ( 'set', 'SYSTEM', 'GMT' )
         CALL CHCKXC ( .TRUE., 'SPICE(BADDEFAULTVALUE)',   OK   )
 
C
C     Reset the defaults
C
      CALL TIMDEF ( 'SET', 'SYSTEM',   'UTC'       )
      CALL TIMDEF ( 'SET', 'CALENDAR', 'GREGORIAN' )
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
