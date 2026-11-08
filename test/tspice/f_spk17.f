C$Procedure F_SPK17 ( Family of tests for the SPK type 17 code)

      SUBROUTINE F_SPK17 ( OK )

C$ Abstract
C
C     This routine tests the writing and reading of type 17
C     SPK segments.
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
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
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 05-JUN-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 1.0.0, 26-MAY-1998 (WLT)
C
C-&


C$ Index_Entries
C
C     None.
C
C-&


C
C     SPICELIB Functions
C
      DOUBLE PRECISION      RPD

C
C     Local Variables
C
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

C
C     Begin every test family with an open call.
C

      CALL TOPEN ( 'F_SPK17' )

C
C     First delete our sample files (if they exists) and
C     open our first new file for writing.
C
      CALL KILFIL ( 'spk17.bsp'   )
      CALL KILFIL ( 'spk17_2.bsp' )

      CALL SPKOPN ( 'spk17.bsp', 'TEST', 1000, HANDLE )

C
C     Here are some constants we'll want later, 10 and
C     5 degrees/day.
C
      FIVDPD  = (  5.0D0 / 86400.0D0 ) * RPD()
      TENDPD  = ( 10.0D0 / 86400.0D0 ) * RPD()


      CALL TCASE ( 'Make sure that an error is signaled if the '
     .//           'semi-major axis is non-positive. ' )


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


         CALL SPKW17 ( HANDLE, BODY,  CENTER, FRAME, FIRST, LAST,
     .                 SEGID,  T0,    EQEL,   RAPOL, DECPOL )


         CALL CHCKXC ( .TRUE., 'SPICE(BADSEMIAXIS)',  OK )


      CALL TCASE ( 'Make sure an error is signaled if the '
     .//           'eccentricity is greater than 0.9 ' )


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


         CALL SPKW17 ( HANDLE, BODY,  CENTER, FRAME, FIRST, LAST,
     .                 SEGID,  T0,    EQEL,   RAPOL, DECPOL )


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



C
C        We want a rate for the node of 10 degrees/day and
C        for the argument of periapse of 5 degrees/day.
C
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


         CALL SPKW17 ( HANDLE, BODY,  CENTER, FRAME, FIRST, LAST,
     .                 SEGID,  T0,    EQEL,   RAPOL, DECPOL )

         CALL SPKCLS ( HANDLE )

         CALL CHCKXC ( .FALSE., ' ',  OK )


         CALL SPKLEF ( 'spk17.bsp', SPKHAN )

         CALL SPKSFS ( BODY, ET, HANDLE, DESCR,  SEGID, FOUND )

         CALL SPKUDS ( DESCR, BODY,   CENTER, REF, TYPE,
     .                 FIRST, LAST,   BEGIN,  END   )

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

      CALL TCASE ( 'Make sure we can successfully subset this '
     .//           'the segment we just created. ' )

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
C
C        Make sure no errors have occurred in the subsetting
C        portion of the code.
C
         CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        Now load the new file and make sure we get correct states.
C

         CALL SPKLEF ( 'spk17_2.bsp', SPKH2 )
C
C        First make sure that we don't find a segment when one
C        is not available.
C
         ET = 1.1D8
         CALL SPKSFS ( BODY, ET, HANDLE, DESCR,  SEGID, FOUND )

         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )

         ET = -1.1D8
         CALL SPKSFS ( BODY, ET, HANDLE, DESCR,  SEGID, FOUND )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
C
C        Next make sure we get correct states.
C
         ET = 0.0D0
         CALL SPKSFS ( BODY, ET, HANDLE, DESCR,  SEGID, FOUND )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

         CALL SPKUDS ( DESCR, BODY,   CENTER, REF, TYPE,
     .                 FIRST, LAST,   BEGIN,  END   )

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
