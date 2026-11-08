C$Procedure F_TPARTV2 ( Family of tests for TPARTV )
 
      SUBROUTINE F_TPARTV2 ( OK )
 
C$ Abstract
C
C     This routine checks that TPARTV can parse the strings
C     that were originally recognized by TPARSE as well as
C     a host of other standard time representations.
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
C     None.
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
C     E.D. Wright      (JPL)
C
C$ Version
C
C-    TSPICE Version 1.3.0, 22-DEC-2021  (EDW)
C
C        Added missing header sections.
C
C-    TSPICE Version 1.2.0, 17-MAR-2009  (EDW)
C
C         F_TPARTV1 and F_TPARTV2 now separate test families.
C
C-    TSPICE Version 1.1.0, 20-AUG-2000  (EDW)
C
C        F_TAPRTV split into routine F_TPARTV1 & F_TPARTV2
C        in order to avoid a compiler problem with the Absoft
C        FORTRAN compiler on a Macintosh PPC.
C
C-&
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C
 
C
C     Local Variables
C
      INTEGER               ERA
      PARAMETER           ( ERA    =  1 )
 
      INTEGER               WDAY
      PARAMETER           ( WDAY   = ERA    + 1 )
 
      INTEGER               ZONE
      PARAMETER           ( ZONE   = WDAY   + 1 )
 
      INTEGER               AMPM
      PARAMETER           ( AMPM   = ZONE   + 1 )
 
      INTEGER               SYSTEM
      PARAMETER           ( SYSTEM = AMPM   + 1 )
 
      INTEGER               NMODS
      PARAMETER           ( NMODS = SYSTEM )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    STRING
 
      INTEGER               LONGSZ
      PARAMETER           ( LONGSZ = 300 )
 
      CHARACTER*(LONGSZ)    ERROR
      CHARACTER*(LONGSZ)    EERROR
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(LNSIZE)    PICTUR
      CHARACTER*(LNSIZE)    PIC
 
      CHARACTER*(WDSIZE)    TYPE
      CHARACTER*(WDSIZE)    ETYPE
      CHARACTER*(WDSIZE)    MODIFY ( NMODS )
      CHARACTER*(WDSIZE)    EMODFY ( NMODS )
 
      DOUBLE PRECISION      TVEC  ( 7 )
      DOUBLE PRECISION      ETVEC ( 7 )
 
      INTEGER               NTVEC
      INTEGER               ENTVEC
 
      LOGICAL               MODS
      LOGICAL               EMODS
      LOGICAL               SUCCES
      LOGICAL               ESUCCS
      LOGICAL               YABBRV
 
 
 
      CALL TOPEN ( 'F_TPARTV2' )

 
      CALL TCASE ( 'Y-M-DTH-M-S ' )
 
         STRING     = '1827-12-27T02:28:27'
         PIC        = 'YYYY-MM-DDTHR:MN:SC'
         ENTVEC     = 6
 
         ETVEC(1)   = 1827.0000000000D0
         ETVEC(2)   = 12.0000000000D0
         ETVEC(3)   = 27.0000000000D0
         ETVEC(4)   = 2.0000000000D0
         ETVEC(5)   = 28.0000000000D0
         ETVEC(6)   = 27.0000000000D0
 
         EMODFY(1) = ' '
         EMODFY(2) = ' '
         EMODFY(3) = ' '
         EMODFY(4) = ' '
         EMODFY(5) = ' '
 
 
 
         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '
         ETYPE       = 'YMD'
 
         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE,
     .                         MODIFY, MODS,  YABBRV,
     .                         SUCCES, PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'PICTUR', PICTUR, '=',  PIC,              OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )
 
         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )


      CALL TCASE ( 'Y-M-DTH-M.xxx ' )
 
         STRING     = '1827-12-27T02:28.281'
         PIC        = 'YYYY-MM-DDTHR:MN.### ::RND'
         ENTVEC     = 5
 
         ETVEC(1)   = 1827.0000000000D0
         ETVEC(2)   = 12.0000000000D0
         ETVEC(3)   = 27.0000000000D0
         ETVEC(4)   = 2.0000000000D0
         ETVEC(5)   = 28.2810000000D0
         ETVEC(6)   = 0.0000000000D0
 
         EMODFY(1) = ' '
         EMODFY(2) = ' '
         EMODFY(3) = ' '
         EMODFY(4) = ' '
         EMODFY(5) = ' '
 
 
 
         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '
         ETYPE       = 'YMD'
 
         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE,
     .                         MODIFY, MODS,  YABBRV,
     .                         SUCCES, PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'PICTUR', PICTUR, '=',  PIC,              OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )
 
         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

 
       CALL TCASE ( 'Y-M-DTH-M.xxx (2)' )
 
         STRING     = '1827-12-27T02:28.277'
         PIC        = 'YYYY-MM-DDTHR:MN.### ::RND'
         ENTVEC     = 5
 
         ETVEC(1)   = 1827.0000000000D0
         ETVEC(2)   = 12.0000000000D0
         ETVEC(3)   = 27.0000000000D0
         ETVEC(4)   = 2.0000000000D0
         ETVEC(5)   = 28.2770000000D0
         ETVEC(6)   = 0.0000000000D0
 
         EMODFY(1) = ' '
         EMODFY(2) = ' '
         EMODFY(3) = ' '
         EMODFY(4) = ' '
         EMODFY(5) = ' '
 
 
 
         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '
         ETYPE       = 'YMD'
 
       CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE,
     .                         MODIFY, MODS,  YABBRV,
     .                         SUCCES, PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'PICTUR', PICTUR, '=',  PIC,              OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )
 
         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )
 
 
      CALL TCASE ( 'Y-DOY/' )
 
         STRING     = '1996-27/'
         PIC        = 'YYYY-DOY/'
         ENTVEC     = 2
 
         ETVEC(1)   = 1996.0000000000D0
         ETVEC(2)   = 27.0000000000D0
         ETVEC(3)   = 0.0000000000D0
         ETVEC(4)   = 0.0000000000D0
         ETVEC(5)   = 0.0000000000D0
 
         EMODFY(1) = ' '
         EMODFY(2) = ' '
         EMODFY(3) = ' '
         EMODFY(4) = ' '
         EMODFY(5) = ' '
 
 
 
         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '
         ETYPE       = 'YD'

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE,
     .                         MODIFY, MODS,  YABBRV,
     .                         SUCCES, PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  5, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'PICTUR', PICTUR, '=',  PIC,              OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )
 
         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )
 
 
      CALL TCASE ( 'YYYY-DOY/H:M ' )
 
         STRING     = '1996-288/16:03'
         PIC        = 'YYYY-DOY/HR:MN'
         ENTVEC     = 4
 
         ETVEC(1)   = 1996.0000000000D0
         ETVEC(2)   = 288.0000000000D0
         ETVEC(3)   = 16.0000000000D0
         ETVEC(4)   = 3.0000000000D0
         ETVEC(5)   = 0.0000000000D0
 
         EMODFY(1) = ' '
         EMODFY(2) = ' '
         EMODFY(3) = ' '
         EMODFY(4) = ' '
         EMODFY(5) = ' '
 
 
 
         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '
         ETYPE       = 'YD'
 
         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE,
     .                         MODIFY, MODS,  YABBRV,
     .                         SUCCES, PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  5, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'PICTUR', PICTUR, '=',  PIC,              OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )
 
         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )


      CALL T_SUCCESS ( OK )
      
      RETURN
      END
