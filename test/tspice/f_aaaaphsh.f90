!$Procedure F_AAAAPHSH ( Family of tests for ZZPHSH )

      SUBROUTINE F_AAAAPHSH ( OK )

      IMPLICIT NONE

! $ Abstract
!
!     This routine tests the POOL and arbitrary divisor hash functions
!     provided by ZZPHSH umbrella. It is named F_AAAAPHSH to ensure
!     given the current TSPICE main module assembly process, it is the
!     first test family executed by TSPICE.
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
!     None
!
! $ Keywords
!
!     None
!
! $ Declarations

      LOGICAL               OK

! $ Brief_I/O
!
!     None
!
! $ Detailed_Input
!
!     None
!
! $ Detailed_Output
!
!     None
!
! $ Parameters
!
!     None
!
! $ Exceptions
!
!     None
!
! $ Files
!
!     None
!
! $ Particulars
!
!     None
!
! $ Examples
!
!     None
!
! $ Restrictions
!
!     None
!
! $ Literature_References
!
!     None
!
! $ Author_and_Institution
!
!     None
!
! $ Version
!
!-    Version 1.0.0  31-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     None
!
!-&

!
!     Test Utility Functions
!

!
!     SPICELIB Functions
!
      INTEGER               INTMAX

      INTEGER               ZZPHSH
      INTEGER               ZZSHSH
      INTEGER               ZZHASH
      INTEGER               ZZHASH2

!
!     Local parameters
!

!
!     Local Variables
!
      INTEGER               M2
      INTEGER               MAXVAR
      INTEGER               TMPINT

      LOGICAL               FOUND

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_AAAAPHSH' )

!
!     NO-OP: calling ZZPHSH.
!
      CALL TCASE ( 'Call ZZPHSH umbrella directly.' )

      TMPINT = ZZPHSH ( ' ', 1, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZPHSH', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZPHSH ( 'BOO', 11, 11 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZPHSH', TMPINT, '=', 0, 0, OK )

!
!     The test below cannot be done because CSTART called by TOPEN
!     already initialized the POOL via CLPOOL that called ZZSHSH.
!
!     ERROR: calling ZZHASH prior to ZZSHSH, try 1.
!
!      CALL TCASE ( 'Call ZZHASH prior to ZZSHSH, try 1.' )
!
!      TMPINT = ZZHASH ( ' ' )
!      CALL CHCKXC ( .TRUE., 'SPICE(CALLEDOUTOFORDER)', OK )
!      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 0, 0, OK )

!
!     ERROR: calling ZZSHSH with bad divisors.
!
      CALL TCASE ( 'Call ZZSHSH with invalid divisors' )

      TMPINT = ZZSHSH ( -1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK )
      CALL CHCKSI ( 'ZZSHSH', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZSHSH ( 0 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK )
      CALL CHCKSI ( 'ZZSHSH', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZSHSH ( INTMAX() )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK )
      CALL CHCKSI ( 'ZZSHSH', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZSHSH ( INTMAX()/68 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK )
      CALL CHCKSI ( 'ZZSHSH', TMPINT, '=', 0, 0, OK )

!
!     The test below cannot be done because CSTART called by TOPEN
!     already initialized the POOL via CLPOOL that called ZZSHSH.
!
!     ERROR: calling ZZHASH prior to ZZSHSH, try 2. Although
!     ZZSHSH was already called, it never got to save the
!     POOL hash divisor.
!
!      CALL TCASE ( 'Call ZZHASH prior to ZZSHSH, try 2.' )
!
!      TMPINT = ZZHASH ( ' ' )
!      CALL CHCKXC ( .TRUE., 'SPICE(CALLEDOUTOFORDER)', OK )
!      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 0, 0, OK )

!
!     ERROR: calling ZZHASH2 with bad divisors.
!
      CALL TCASE ( 'Call ZZHASH2 with invalid divisors' )

      TMPINT = ZZHASH2 ( ' ', -1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZHASH2 ( ' ', 0 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZHASH2 ( ' ', INTMAX() )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 0, 0, OK )

      TMPINT = ZZHASH2 ( ' ', INTMAX()/68 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 0, 0, OK )

!
!     The test below cannot be done because CSTART called by TOPEN
!     already initialized the POOL via CLPOOL that called ZZSHSH.
!
!     ERROR: calling ZZHASH prior to ZZSHSH, try 3.
!
!      CALL TCASE ( 'Call ZZHASH prior to ZZSHSH, try 3.' )
!
!      TMPINT = ZZHASH ( ' ' )
!      CALL CHCKXC ( .TRUE., 'SPICE(CALLEDOUTOFORDER)', OK )
!      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 0, 0, OK )

!
!     GOOD: check a few ZZHASH2 values.
!
      CALL TCASE ( 'Call ZZHASH2 with a few good values.' )

      M2 = 1

      TMPINT = ZZHASH2 ( ' ', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 1, 0, OK )

      TMPINT = ZZHASH2 ( 'Hakuna-Matata!', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 1, 0, OK )

      M2 = 5003

      TMPINT = ZZHASH2 ( 'A', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 749, 0, OK )

      TMPINT = ZZHASH2 ( 'A A', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 749, 0, OK )

      TMPINT = ZZHASH2 ( 'a A', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 749, 0, OK )

      TMPINT = ZZHASH2 ( 'AA', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 1583, 0, OK )

      TMPINT = ZZHASH2 ( 'AA AA', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 1583, 0, OK )

      TMPINT = ZZHASH2 ( 'aa AA', M2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZHASH2', TMPINT, '=', 1583, 0, OK )

!
!     The test below cannot be done because CSTART called by TOPEN
!     already initialized the POOL via CLPOOL that called ZZSHSH.
!
!     ERROR: calling ZZHASH prior to ZZSHSH, try 4. Although
!     ZZHASH2 initialized character-code table, the POOL
!     hash divisor is still uninitialized.
!
!      CALL TCASE ( 'Call ZZHASH prior to ZZSHSH, try 4.' )
!
!      TMPINT = ZZHASH ( ' ' )
!      CALL CHCKXC ( .TRUE., 'SPICE(CALLEDOUTOFORDER)', OK )
!      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 0, 0, OK )

!
!     GOOD: finally call ZZSHSH. Use MAXVAR of the POOL for
!     consistency.
!
      CALL TCASE ( 'Call ZZSHSH with a good divisor.' )

      CALL SZPOOL ( 'MAXVAR', MAXVAR, FOUND )
      CALL CHCKSL ( 'POOL MAXVAR FOUND', FOUND, .TRUE.,  OK )

      TMPINT = ZZSHSH ( MAXVAR )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZSHSH', TMPINT, '=', 0, 0, OK )

!
!     GOOD: check a few ZZHASH values.
!
      CALL TCASE ( 'Call ZZHASH with a few good values.' )

      TMPINT = ZZHASH ( 'A' )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 749, 0, OK )

      TMPINT = ZZHASH ( 'A A' )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 749, 0, OK )

      TMPINT = ZZHASH ( 'a A' )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'ZZHASH', TMPINT, '=', 749, 0, OK )

!
!     This is good enough for now.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
