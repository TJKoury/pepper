!$Procedure F_TPARTV1 ( Family of tests for TPARTV )

      SUBROUTINE F_TPARTV1 ( OK )

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
!     B.V. Semenov     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.4.0, 22-DEC-2021  (EDW)
!
!        Added missing header sections.
!
!-    TSPICE Version 1.3.0, 09-NOV-2020 (BVS)
!
!        Removed blank before 'D' in literal DP constants.
!
!        Added missing header sections.
!
!-    TSPICE Version 1.2.0, 17-MAR-2009  (EDW)
!
!        F_TPARTV1 and F_TPARTV2 now separate test families.
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

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_TPARTV1' )

!
!     We are first going to check on q group of strings that we
!     think should be parsable.
!
      CALL TCASE ( 'YMDHMS' )

         STRING = '1994 Mar 12 12:28:18.281 '

         ETVEC ( 1 ) = 1994.0D0
         ETVEC ( 2 ) =    3.0D0
         ETVEC ( 3 ) =   12.0D0
         ETVEC ( 4 ) =   12.0D0
         ETVEC ( 5 ) =   28.0D0
         ETVEC ( 6 ) =   18.281D0

         ENTVEC      =  6
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '


         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=', 'YMD',             OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )



      CALL TCASE ( 'YMDHMS' )

         STRING = '1994 3 12 12:18:18.282 TDT '
         ETVEC ( 1 ) = 1994.0D0
         ETVEC ( 2 ) =    3.0D0
         ETVEC ( 3 ) =   12.0D0
         ETVEC ( 4 ) =   12.0D0
         ETVEC ( 5 ) =   18.0D0
         ETVEC ( 6 ) =   18.282D0

         ENTVEC      =  6
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = 'TDT'


         EMODS       = .TRUE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=', 'YMD',             OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'DMYHMS' )

         STRING = '3/APR/57 A.D. 12:18.1981 TDB '
         ETVEC ( 1 ) =   57.0D0
         ETVEC ( 2 ) =    4.0D0
         ETVEC ( 3 ) =    3.0D0
         ETVEC ( 4 ) =   12.0D0
         ETVEC ( 5 ) =   18.1981D0
         ETVEC ( 6 ) =   0.0D0

         ENTVEC      =  5
         ETYPE       = 'YMD'

         EMODFY(1)   = 'A.D.'
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = 'TDB'


         EMODS       = .TRUE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=', 'YMD',             OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'MDYHMS' )

         STRING = '3/28/''96 18:28:28.289 PDT '

         ETVEC ( 1 ) =   96.0D0
         ETVEC ( 2 ) =    3.0D0
         ETVEC ( 3 ) =   28.0D0
         ETVEC ( 4 ) =   18.0D0
         ETVEC ( 5 ) =   28.0D0
         ETVEC ( 6 ) =   28.289D0

         ENTVEC      =  6
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '
         EMODFY(ZONE)= 'UTC-7'


         EMODS       = .TRUE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=', 'YMD',             OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'MDHMSY' )

         STRING = 'Mon Apr 22 09:40:36 PDT 1996 '

         ETVEC ( 1 ) = 1996.0D0
         ETVEC ( 2 ) =    4.0D0
         ETVEC ( 3 ) =   22.0D0
         ETVEC ( 4 ) =    9.0D0
         ETVEC ( 5 ) =   40.0D0
         ETVEC ( 6 ) =   36.0D0

         ENTVEC      =  6
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '
         EMODFY(ZONE)= 'UTC-7'
         EMODFY(WDAY)= 'MON'



         EMODS       = .TRUE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=', 'YMD',             OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'YDHMS' )

         STRING = '1918-171/ 03:28:57.1819 (UTC) '

         ETVEC ( 1 ) = 1918.0D0
         ETVEC ( 2 ) =  171.0D0
         ETVEC ( 3 ) =   03.0D0
         ETVEC ( 4 ) =   28.0D0
         ETVEC ( 5 ) =   57.1819D0

         ENTVEC      =  5
         ETYPE       = 'YD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '
         EMODFY(SYSTEM)= 'UTC'



         EMODS       = .TRUE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  5, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'YDHMS' )
         STRING = '1986 32// 02:18:09. CDT '

         ETVEC ( 1 ) = 1986.0D0
         ETVEC ( 2 ) =   32.0D0
         ETVEC ( 3 ) =   02.0D0
         ETVEC ( 4 ) =   18.0D0
         ETVEC ( 5 ) =   09.0D0

         ENTVEC      =  5
         ETYPE       = 'YD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '
         EMODFY(ZONE)= 'UTC-5'


         EMODS       = .TRUE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  5, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )


      CALL TCASE ( 'JD' )

         STRING = '2441889.18997917 JDUTC '

         ETVEC ( 1 ) = 2441889.18997917D0

         ENTVEC      =  1
         ETYPE       = 'JD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '
         EMODFY(SYSTEM)= 'UTC'


         EMODS       = .TRUE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  1, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )


      CALL TCASE ( 'JD' )
         STRING = '2451545.5000000 JDTDB '

         ETVEC ( 1 ) = 2451545.5000000D0

         ENTVEC      =  1
         ETYPE       = 'JD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '
         EMODFY(SYSTEM)= 'TDB'


         EMODS       = .TRUE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  1, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )


      CALL TCASE ( 'JD' )
         STRING = '2451792.1827191 JD '

         ETVEC ( 1 ) = 2451792.1827191D0

         ENTVEC      =  1
         ETYPE       = 'JD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '


         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  1, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )


      CALL TCASE ( 'JD' )

         STRING = '2431829.28719 (JD) '
         ETVEC ( 1 ) = 2431829.28719D0

         ENTVEC      =  1
         ETYPE       = 'JD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '


         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  1, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )


      CALL TCASE ( 'ISO1' )

         STRING = '1992-12-18T12:18:18 '

         ETVEC ( 1 ) = 1992.0D0
         ETVEC ( 2 ) =   12.0D0
         ETVEC ( 3 ) =   18.0D0
         ETVEC ( 4 ) =   12.0D0
         ETVEC ( 5 ) =   18.0D0
         ETVEC ( 6 ) =   18.0D0

         ENTVEC      =  6
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=', 'YMD',             OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'ISO2' )

         STRING = '1995-12T '

         ETVEC ( 1 ) = 1995.0D0
         ETVEC ( 2 ) =   12.0D0
         ETVEC ( 3 ) =    0.0D0
         ETVEC ( 4 ) =    0.0D0
         ETVEC ( 5 ) =    0.0D0

         ENTVEC      =  2
         ETYPE       = 'YD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  5, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'ISO3' )

         STRING = '1996-1-18T '

         ETVEC ( 1 ) = 1996.0D0
         ETVEC ( 2 ) =    1.0D0
         ETVEC ( 3 ) =   18.0D0
         ETVEC ( 4 ) =    0.0D0
         ETVEC ( 5 ) =    0.0D0
         ETVEC ( 6 ) =    0.0D0

         ENTVEC      =  3
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'ISO4' )

         STRING = '1996-3-18T12 '

         ETVEC ( 1 ) = 1996.0D0
         ETVEC ( 2 ) =    3.0D0
         ETVEC ( 3 ) =   18.0D0
         ETVEC ( 4 ) =   12.0D0
         ETVEC ( 5 ) =    0.0D0
         ETVEC ( 6 ) =    0.0D0

         ENTVEC      =  4
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )

         CALL TSTMSG ( '#', STRING )
         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )
         CALL TSTMSG ( '#', ' ' )

      CALL TCASE ( 'ISO5' )
         STRING = '1996-3-18T12:28.187 '

         ETVEC ( 1 ) = 1996.0D0
         ETVEC ( 2 ) =    3.0D0
         ETVEC ( 3 ) =   18.0D0
         ETVEC ( 4 ) =   12.0D0
         ETVEC ( 5 ) =   28.187D0
         ETVEC ( 6 ) =    0.0D0

         ENTVEC      =  5
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'ISO6' )

         STRING = '1993-172T12:18:18.1879292 '

         ETVEC ( 1 ) = 1993.0D0
         ETVEC ( 2 ) =  172.0D0
         ETVEC ( 3 ) =   12.0D0
         ETVEC ( 4 ) =   18.0D0
         ETVEC ( 5 ) =   18.1879292D0

         ENTVEC      =  5
         ETYPE       = 'YD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  5, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'YMD' )
         STRING = '105 B.C. 3/4 '

         ETVEC ( 1 ) =  105.0D0
         ETVEC ( 2 ) =    3.0D0
         ETVEC ( 3 ) =    4.0D0
         ETVEC ( 4 ) =    0.0D0
         ETVEC ( 5 ) =    0.0D0
         ETVEC ( 6 ) =    0.0D0

         ENTVEC      =  3
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '
         EMODFY(ERA) = 'B.C.'

         EMODS       = .TRUE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'YDM' )
         STRING = '18 A.D. 4 March '

         ETVEC ( 1 ) =   18.0D0
         ETVEC ( 2 ) =    3.0D0
         ETVEC ( 3 ) =    4.0D0
         ETVEC ( 4 ) =    0.0D0
         ETVEC ( 5 ) =    0.0D0
         ETVEC ( 6 ) =    0.0D0

         ENTVEC      =  3
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '
         EMODFY(ERA) = 'A.D.'

         EMODS       = .TRUE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'YMDHMS' )
         STRING = '1919 12 02 12 18 19.19 '

         ETVEC ( 1 ) =  1919.0D0
         ETVEC ( 2 ) =    12.0D0
         ETVEC ( 3 ) =    02.0D0
         ETVEC ( 4 ) =    12.0D0
         ETVEC ( 5 ) =    18.0D0
         ETVEC ( 6 ) =    19.19D0

         ENTVEC      =  6
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'YHMSDM' )
         STRING = '1991 12:18:03.182 7 JAN '

         ETVEC ( 1 ) =  1991.0D0
         ETVEC ( 2 ) =     1.0D0
         ETVEC ( 3 ) =    07.0D0
         ETVEC ( 4 ) =    12.0D0
         ETVEC ( 5 ) =    18.0D0
         ETVEC ( 6 ) =    03.182D0

         ENTVEC      =  6
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'HMSMDY' )
         STRING = '12:18:07 March 18, 1828 '


         ETVEC ( 1 ) =  1828.0D0
         ETVEC ( 2 ) =     3.0D0
         ETVEC ( 3 ) =    18.0D0
         ETVEC ( 4 ) =    12.0D0
         ETVEC ( 5 ) =    18.0D0
         ETVEC ( 6 ) =    07.0D0

         ENTVEC      =  6
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'DYHMS' )
         STRING = '261-1998/17:18:21.1879 '


         ETVEC ( 1 ) =  1998.0D0
         ETVEC ( 2 ) =   261.0D0
         ETVEC ( 3 ) =    17.0D0
         ETVEC ( 4 ) =    18.0D0
         ETVEC ( 5 ) =    21.1879D0

         ENTVEC      =  5
         ETYPE       = 'YD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  5, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )

      CALL TCASE ( 'DYHMS' )
         STRING = '217-1998::12:18:21.2987 '


         ETVEC ( 1 ) =  1998.0D0
         ETVEC ( 2 ) =   217.0D0
         ETVEC ( 3 ) =    12.0D0
         ETVEC ( 4 ) =    18.0D0
         ETVEC ( 5 ) =    21.2987D0

         ENTVEC      =  5
         ETYPE       = 'YD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  5, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )


      CALL TCASE ( 'DYHMS' )

         STRING = '178-1872//12:18:17.1879 '


         ETVEC ( 1 ) =  1872.0D0
         ETVEC ( 2 ) =   178.0D0
         ETVEC ( 3 ) =    12.0D0
         ETVEC ( 4 ) =    18.0D0
         ETVEC ( 5 ) =    17.1879D0

         ENTVEC      =  5
         ETYPE       = 'YD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  5, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )



      CALL TCASE ( 'YMDHMS' )

         STRING = '1982-JAN 57 12:72:93.2987 '


         ETVEC ( 1 ) =  1982.0D0
         ETVEC ( 2 ) =     1.0D0
         ETVEC ( 3 ) =    57.0D0
         ETVEC ( 4 ) =    12.0D0
         ETVEC ( 5 ) =    72.0D0
         ETVEC ( 6 ) =    93.2987D0

         ENTVEC      =  6
         ETYPE       = 'YMD'

         EMODFY(1)   = ' '
         EMODFY(2)   = ' '
         EMODFY(3)   = ' '
         EMODFY(4)   = ' '
         EMODFY(5)   = ' '

         EMODS       = .FALSE.
         ESUCCS      = .TRUE.
         EERROR      = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )


         CALL CHCKXC ( .FALSE., ' ',                             OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC,  6, 0.0D0, OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',  ENTVEC, 0,        OK )
         CALL CHCKSC ( 'TYPE',   TYPE,   '=',  ETYPE,            OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',  EERROR,           OK )
         CALL CHCKSL ( 'MODS',   MODS,         EMODS,            OK )
         CALL CHCKSL ( 'SUCCES', SUCCES,       ESUCCS,           OK )

         CALL CHCKSC ( 'MODIFY(1)', MODIFY(1), '=',  EMODFY(1),  OK )
         CALL CHCKSC ( 'MODIFY(2)', MODIFY(2), '=',  EMODFY(2),  OK )
         CALL CHCKSC ( 'MODIFY(3)', MODIFY(3), '=',  EMODFY(3),  OK )
         CALL CHCKSC ( 'MODIFY(4)', MODIFY(4), '=',  EMODFY(4),  OK )
         CALL CHCKSC ( 'MODIFY(5)', MODIFY(5), '=',  EMODFY(5),  OK )



      CALL TCASE ( 'UNIX Date/Time' )

         STRING     = 'Tue Apr 30 09:08:46 PDT 1996'
         PIC        = 'Wkd Mon DD HR:MN:SC PDT YYYY ::UTC-7'
         ENTVEC     = 6

         ETVEC(1)   = 1996.0000000000D0
         ETVEC(2)   = 4.0000000000D0
         ETVEC(3)   = 30.0000000000D0
         ETVEC(4)   = 9.0000000000D0
         ETVEC(5)   = 8.0000000000D0
         ETVEC(6)   = 46.0000000000D0

         EMODFY(1) = ' '
         EMODFY(2) = 'TUE'
         EMODFY(3) = 'UTC-7'
         EMODFY(4) = ' '
         EMODFY(5) = ' '



         EMODS       = .TRUE.
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


      CALL TCASE ( 'UNIX MAIL Time Stamp' )

         STRING     = 'Tue, Apr 30 09:08:46 PDT 1996'
         PIC        = 'Wkd, Mon DD HR:MN:SC PDT YYYY ::UTC-7'
         ENTVEC     = 6

         ETVEC(1)   = 1996.0000000000D0
         ETVEC(2)   = 4.0000000000D0
         ETVEC(3)   = 30.0000000000D0
         ETVEC(4)   = 9.0000000000D0
         ETVEC(5)   = 8.0000000000D0
         ETVEC(6)   = 46.0000000000D0

         EMODFY(1) = ' '
         EMODFY(2) = 'TUE'
         EMODFY(3) = 'UTC-7'
         EMODFY(4) = ' '
         EMODFY(5) = ' '



         EMODS       = .TRUE.
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


      CALL TCASE ('UNIX Date/Time Abbreviated')

         STRING = 'Tue Apr 30 09:08:46 PDT 96'
         PIC    = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )

         CALL CHCKXC ( .FALSE.,' ',                 OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=', 0, 0, OK )
         CALL CHCKSL ( 'SUCCES', SUCCES, .FALSE.,   OK )
         CALL CHCKSC ( 'PICTUR', PICTUR, '=',  PIC, OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=', ' ', OK )


      CALL TCASE ( 'Leading Delimiter ' )

         STRING = '-1996 Jan 12'
         PIC    = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )

         CALL CHCKXC ( .FALSE.,' ',                 OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=', 0, 0, OK )
         CALL CHCKSL ( 'SUCCES', SUCCES, .FALSE.,   OK )
         CALL CHCKSC ( 'PICTUR', PICTUR, '=',  PIC, OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=', ' ', OK )


      CALL TCASE ( 'Trailing Delimiter ' )

         STRING = '1997 Jan 12 ,'
         PIC    = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )

         CALL CHCKXC ( .FALSE.,' ',                 OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=', 0, 0, OK )
         CALL CHCKSL ( 'SUCCES', SUCCES, .FALSE.,   OK )
         CALL CHCKSC ( 'PICTUR', PICTUR, '=',  PIC, OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=', ' ', OK )


      CALL TCASE ( 'YMDT ' )

         STRING     = '1996-12-18T'
         PIC        = 'YYYY-MM-DDT'
         ENTVEC     = 3

         ETVEC(1)   = 1996.0000000000D0
         ETVEC(2)   = 12.0000000000D0
         ETVEC(3)   = 18.0000000000D0
         ETVEC(4)   = 0.0000000000D0
         ETVEC(5)   = 0.0000000000D0
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


      CALL TCASE ( 'YMDTHM ' )

         STRING     = '1827-12-27T02:28'
         PIC        = 'YYYY-MM-DDTHR:MN'
         ENTVEC     = 5

         ETVEC(1)   = 1827.0000000000D0
         ETVEC(2)   = 12.0000000000D0
         ETVEC(3)   = 27.0000000000D0
         ETVEC(4)   = 2.0000000000D0
         ETVEC(5)   = 28.0000000000D0
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


      CALL TCASE ( 'YMDTHM and trailing colon ' )

         STRING = '1827-12-27T02:28:'
         PIC    = ' '

         CALL TPARTV ( STRING, TVEC,   NTVEC, TYPE, &
                               MODIFY, MODS,  YABBRV, &
                               SUCCES, PICTUR, ERROR )

         CALL CHCKXC ( .FALSE.,' ',                 OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=', 0, 0, OK )
         CALL CHCKSL ( 'SUCCES', SUCCES, .FALSE.,   OK )
         CALL CHCKSC ( 'PICTUR', PICTUR, '=',  PIC, OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=', ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
