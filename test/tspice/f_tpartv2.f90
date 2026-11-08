!$Procedure F_TPARTV2 ( Family of tests for TPARTV )

      SUBROUTINE F_TPARTV2 ( OK )

! $ Abstract
!
!     This routine checks that TPARTV can parse the strings
!     that were originally recognized by TPARSE as well as
!     a host of other standard time representations.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
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
!     E.D. Wright      (JPL)
!
! $ Version
!
!-    TSPICE Version 1.3.0, 22-DEC-2021  (EDW)
!
!        Added missing header sections.
!
!-    TSPICE Version 1.2.0, 17-MAR-2009  (EDW)
!
!         F_TPARTV1 and F_TPARTV2 now separate test families.
!
!-    TSPICE Version 1.1.0, 20-AUG-2000  (EDW)
!
!        F_TAPRTV split into routine F_TPARTV1 & F_TPARTV2
!        in order to avoid a compiler problem with the Absoft
!        FORTRAN compiler on a Macintosh PPC.
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!

!
!     Local Variables
!
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

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


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

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


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

       CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


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

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


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

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


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
