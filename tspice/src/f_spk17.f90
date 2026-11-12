!$Procedure F_SPK17 ( Family of tests for the SPK type 17 code)

      SUBROUTINE F_SPK17 ( OK )
      use pepper_legacy, pepper_legacy_frame => frame

! $ Abstract
!
!     This routine tests the writing and reading of type 17
!     SPK segments.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
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
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 05-JUN-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 1.0.0, 26-MAY-1998 (WLT)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     SPICELIB Functions
!

!
!     Local Variables
!
      INTEGER               SEGSIZ
      PARAMETER           ( SEGSIZ = 40 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(SEGSIZ)    SEGID
      CHARACTER*(WDSIZE)    FRAME

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ARGP
      DOUBLE PRECISION      DECPOL
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      EQEL(9)
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIVDPD
      DOUBLE PRECISION      GM
      DOUBLE PRECISION      INC
      DOUBLE PRECISION      M0
      DOUBLE PRECISION      N
      DOUBLE PRECISION      NODE
      DOUBLE PRECISION      P
      DOUBLE PRECISION      RAPOL
      DOUBLE PRECISION      T0
      DOUBLE PRECISION      TENDPD
      DOUBLE PRECISION      STATE1 ( 6 )
      DOUBLE PRECISION      STATE2 ( 6 )
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST

      DOUBLE PRECISION      DESCR  ( 5 )

      INTEGER               BEGIN
      INTEGER               BODY
      INTEGER               CENTER
      INTEGER               END
      INTEGER               HANDLE
      INTEGER               SPKHAN
      INTEGER               SPKH2
      INTEGER               I
      INTEGER               NELTS
      INTEGER               NEWH
      INTEGER               REF
      INTEGER               TYPE

      LOGICAL               FOUND

!
!     Begin every test family with an open call.
!

      CALL TOPEN ( 'F_SPK17' )

!
!     First delete our sample files (if they exists) and
!     open our first new file for writing.
!
      CALL KILFIL ( 'spk17.bsp'   )
      CALL KILFIL ( 'spk17_2.bsp' )

      CALL SPKOPN ( 'spk17.bsp', 'TEST', 1000, HANDLE )

!
!     Here are some constants we'll want later, 10 and
!     5 degrees/day.
!
      FIVDPD  = (  5.0D0 / 86400.0D0 ) * RPD()
      TENDPD  = ( 10.0D0 / 86400.0D0 ) * RPD()


      CALL TCASE ( 'Make sure that an error is signaled if the ' &
      //           'semi-major axis is non-positive. ' )


         P    =      1.0D4
         GM   = 398600.436D0
         ECC  =      0.1D0
         A    = P/( 1.0D0 - ECC )
         N    = DSQRT ( GM / A  ) / A
         A    = -A
         ARGP = 30.0D0 * RPD()
         NODE = 15.0D0 * RPD()
         INC  = 10.0D0 * RPD()
         M0   = 45.0D0 * RPD()
         T0   = -100000000.0D0

         EQEL(1) = A
         EQEL(2) = ECC*DSIN(ARGP+NODE)
         EQEL(3) = ECC*DCOS(ARGP+NODE)
         EQEL(4) = M0 + ARGP + NODE
         EQEL(5) = DTAN(INC/2.0D0)*DSIN(NODE)
         EQEL(6) = DTAN(INC/2.0D0)*DCOS(NODE)
         EQEL(7) = FIVDPD + TENDPD
         EQEL(8) = N + FIVDPD + TENDPD
         EQEL(9) = TENDPD

         RAPOL   = 30.D0 * RPD()
         DECPOL  = 60.D0 * RPD()

         BODY   = -1000
         SEGID  = 'PHOENIX'
         CENTER = 399
         FRAME  = 'B1950'

         FIRST  = -1.0D9
         LAST   =  1.0D9
         ET     =  0.0D0


         CALL SPKW17 ( HANDLE, BODY,  CENTER, FRAME, FIRST, LAST, &
                       SEGID,  T0,    EQEL,   RAPOL, DECPOL )


         CALL CHCKXC ( .TRUE., 'SPICE(BADSEMIAXIS)',  OK )


      CALL TCASE ( 'Make sure an error is signaled if the ' &
      //           'eccentricity is greater than 0.9 ' )


         P    =      1.0D4
         GM   = 398600.436D0
         ECC  =      0.91D0
         A    = P/( 1.0D0 - ECC )
         N    = DSQRT ( GM / A ) / A
         ARGP = 30.0D0 * RPD()
         NODE = 15.0D0 * RPD()
         INC  = 10.0D0 * RPD()
         M0   = 45.0D0 * RPD()
         T0   = -100000000.0D0

         EQEL(1) = A
         EQEL(2) = ECC*DSIN(ARGP+NODE)
         EQEL(3) = ECC*DCOS(ARGP+NODE)
         EQEL(4) = M0 + ARGP + NODE
         EQEL(5) = DTAN(INC/2.0D0)*DSIN(NODE)
         EQEL(6) = DTAN(INC/2.0D0)*DCOS(NODE)
         EQEL(7) = FIVDPD + TENDPD
         EQEL(8) = N + FIVDPD + TENDPD
         EQEL(9) = TENDPD

         RAPOL   = 30.D0 * RPD()
         DECPOL  = 60.D0 * RPD()

         BODY   = -1000
         SEGID  = 'PHOENIX'
         CENTER = 399
         FRAME  = 'B1950'

         FIRST  = -1.0D9
         LAST   =  1.0D9
         ET     =  0.0D0


         CALL SPKW17 ( HANDLE, BODY,  CENTER, FRAME, FIRST, LAST, &
                       SEGID,  T0,    EQEL,   RAPOL, DECPOL )


         CALL CHCKXC ( .TRUE., 'SPICE(BADECCENTRICITY)',  OK )



      CALL TCASE ( 'Write and read a type 17 kernel.' )



         P    =      1.0D4
         GM   = 398600.436D0
         ECC  =      0.1D0
         A    = P/( 1.0D0 - ECC )
         N    = DSQRT ( GM / A ) / A
         ARGP = 30.0D0 * RPD()
         NODE = 15.0D0 * RPD()
         INC  = 10.0D0 * RPD()
         M0   = 45.0D0 * RPD()
         T0   = -100000000.0D0



!
!        We want a rate for the node of 10 degrees/day and
!        for the argument of periapse of 5 degrees/day.
!
         FIVDPD  = (  5.0D0 / 86400.0D0 ) * RPD()
         TENDPD  = ( 10.0D0 / 86400.0D0 ) * RPD()

         EQEL(1) = A
         EQEL(2) = ECC*DSIN(ARGP+NODE)
         EQEL(3) = ECC*DCOS(ARGP+NODE)
         EQEL(4) = M0 + ARGP + NODE
         EQEL(5) = DTAN(INC/2.0D0)*DSIN(NODE)
         EQEL(6) = DTAN(INC/2.0D0)*DCOS(NODE)
         EQEL(7) = FIVDPD + TENDPD
         EQEL(8) = N + FIVDPD + TENDPD
         EQEL(9) = TENDPD

         RAPOL   = 30.D0 * RPD()
         DECPOL  = 60.D0 * RPD()

         BODY   = -1000
         SEGID  = 'PHOENIX'
         CENTER = 399
         FRAME  = 'B1950'

         FIRST  = -1.0D9
         LAST   =  1.0D9
         ET     =  0.0D0


         CALL SPKW17 ( HANDLE, BODY,  CENTER, FRAME, FIRST, LAST, &
                       SEGID,  T0,    EQEL,   RAPOL, DECPOL )

         CALL SPKCLS ( HANDLE )

         CALL CHCKXC ( .FALSE., ' ',  OK )


         CALL SPKLEF ( 'spk17.bsp', SPKHAN )

         CALL SPKSFS ( BODY, ET, HANDLE, DESCR,  SEGID, FOUND )

         CALL SPKUDS ( DESCR, BODY,   CENTER, REF, TYPE, &
                       FIRST, LAST,   BEGIN,  END   )

         NELTS = END - BEGIN + 1

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'BODY',   BODY,   '=',   -1000, 0, OK )
         CALL CHCKSI ( 'CENTER', CENTER, '=',     399, 0, OK )
         CALL CHCKSI ( 'FRAME',  REF,    '=',       2, 0, OK )
         CALL CHCKSI ( 'TYPE',   TYPE,   '=',      17, 0, OK )
         CALL CHCKSI ( 'NELTS',  NELTS,  '=',      12, 0, OK )
         CALL CHCKSD ( 'FIRST',  FIRST,  '=',  -1.0D9, 0.0D0, OK )
         CALL CHCKSD ( 'LAST',   LAST,   '=',   1.0D9, 0.0D0, OK )


         ET = T0 - 10000.0D0


         DO I = 1, 100
            ET    =  ET + 250.0D0

            CALL EQNCPV ( ET, T0, EQEL,  RAPOL,   DECPOL, STATE1 )
            CALL SPKPVN ( HANDLE, DESCR, ET, REF, STATE2, CENTER )

            CALL CHCKSI ( 'CENTER', CENTER, '=',     399, 0, OK )
            CALL CHCKSI ( 'FRAME',  REF,    '=',       2, 0, OK )
            CALL CHCKAD ( 'STATE',  STATE1, '=',  STATE2, 6, 0.0D0, OK )

         END DO
         CALL SPKUEF ( SPKHAN )

      CALL TCASE ( 'Make sure we can successfully subset this ' &
      //           'the segment we just created. ' )

         CALL KILFIL ( 'spk17_2.bsp' )

         CALL SPKLEF ( 'spk17.bsp', SPKHAN )
         CALL SPKSFS ( BODY,  ET,   HANDLE, DESCR,  SEGID, FOUND  )
         CALL SPKOPN ( 'spk17_2.bsp', 'TEST2',      1000,  NEWH )

         SEGID = 'PHOENIX-2'

         FIRST = -1.0D8
         LAST  =  1.0D8

         CALL SPKSUB ( HANDLE, DESCR, SEGID, FIRST, LAST, NEWH )
         CALL SPKCLS ( NEWH   )
         CALL SPKUEF ( SPKHAN )
!
!        Make sure no errors have occurred in the subsetting
!        portion of the code.
!
         CALL CHCKXC ( .FALSE., ' ',  OK )

!
!        Now load the new file and make sure we get correct states.
!

         CALL SPKLEF ( 'spk17_2.bsp', SPKH2 )
!
!        First make sure that we don't find a segment when one
!        is not available.
!
         ET = 1.1D8
         CALL SPKSFS ( BODY, ET, HANDLE, DESCR,  SEGID, FOUND )

         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )

         ET = -1.1D8
         CALL SPKSFS ( BODY, ET, HANDLE, DESCR,  SEGID, FOUND )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
!
!        Next make sure we get correct states.
!
         ET = 0.0D0
         CALL SPKSFS ( BODY, ET, HANDLE, DESCR,  SEGID, FOUND )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

         CALL SPKUDS ( DESCR, BODY,   CENTER, REF, TYPE, &
                       FIRST, LAST,   BEGIN,  END   )

         NELTS = END - BEGIN + 1

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'BODY',   BODY,   '=',   -1000, 0, OK )
         CALL CHCKSI ( 'CENTER', CENTER, '=',     399, 0, OK )
         CALL CHCKSI ( 'FRAME',  REF,    '=',       2, 0, OK )
         CALL CHCKSI ( 'TYPE',   TYPE,   '=',      17, 0, OK )
         CALL CHCKSI ( 'NELTS',  NELTS,  '=',      12, 0, OK )
         CALL CHCKSD ( 'FIRST',  FIRST,  '=',  -1.0D8, 0.0D0, OK )
         CALL CHCKSD ( 'LAST',   LAST,   '=',   1.0D8, 0.0D0, OK )


         ET = T0 - 10000.0D0


         DO I = 1, 100
            ET    =  ET + 250.0D0

            CALL EQNCPV ( ET, T0, EQEL,  RAPOL,   DECPOL, STATE1 )
            CALL SPKPVN ( HANDLE, DESCR, ET, REF, STATE2, CENTER )

            CALL CHCKSI ( 'CENTER', CENTER, '=',     399, 0, OK )
            CALL CHCKSI ( 'FRAME',  REF,    '=',       2, 0, OK )
            CALL CHCKAD ( 'STATE',  STATE1, '=',  STATE2, 6, 0.0D0, OK )

         END DO
         CALL SPKUEF ( SPKH2  )
         CALL SPKUEF ( SPKHAN )

      CALL KILFIL ( 'spk17.bsp'   )
      CALL KILFIL ( 'spk17_2.bsp' )
      CALL T_SUCCESS ( OK )
      RETURN
      END
