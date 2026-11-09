!$Procedure F_ZZBODS2C ( Family of tests for ZZBODS2C and its callers )

      SUBROUTINE F_ZZBODS2C ( OK )
      use pepper_inc_zzctr

! $ Abstract
!
!     This routine tests ZZBODS2C and the internal save functionality
!     of all routines that call it.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
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
!     M. Liukis          (JPL)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.1, 25-JUL-2020 (BVS) (ML) (JDR)
!
!        Added test cases for GETFVN routine.
!
!-    TSPICE Version 1.0.0, 31-MAR-2014 (BVS)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     Test Utility Functions
!

!
!     SPICELIB Functions
!
      DOUBLE PRECISION      LSPCN
      DOUBLE PRECISION      PHASEQ

!
!     Local parameters
!
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )

      INTEGER               BUFLEN
      PARAMETER           ( BUFLEN = 6 )

      INTEGER               NCASES
      PARAMETER           ( NCASES = 4 )

      INTEGER               NPTS
      PARAMETER           ( NPTS   = 8 )

      INTEGER               ROOM
      PARAMETER           ( ROOM   = 3 )

!
!     Local Variables
!
      INTEGER               USRCTR ( CTRSIZ )
      INTEGER               SAVCTR ( CTRSIZ )

      CHARACTER*(MAXL)      TARG
      INTEGER               TARGID
      LOGICAL               FOUND

      CHARACTER*(MAXL)      SVTARG
      INTEGER               SVTGID
      LOGICAL               SVFND

      CHARACTER*(LINLEN)    BUFFER ( BUFLEN )

      INTEGER               HANDLE

      DOUBLE PRECISION      ET

      CHARACTER*(MAXL)      BODY1
      CHARACTER*(MAXL)      BODY2
      CHARACTER*(MAXL)      BODY3

      INTEGER               I
      INTEGER               CI

      CHARACTER*(LINLEN)    TITLE

      INTEGER               N      (          NCASES )
      DOUBLE PRECISION      RADII  ( 3,       NCASES )

      DOUBLE PRECISION      F

      DOUBLE PRECISION      JACOB1 ( 3, 3,    NCASES )

      DOUBLE PRECISION      JACOB2 ( 3, 3,    NCASES )

      INTEGER               B1CODE
      INTEGER               FRCODE
      CHARACTER*(MAXL)      FIXREF

      DOUBLE PRECISION      TRGEP1 (          NCASES )
      DOUBLE PRECISION      OBSPO1 ( 3,       NCASES )
      DOUBLE PRECISION      TRMPT1 ( 3, NPTS, NCASES )

      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      PHASE1 (          NCASES )
      DOUBLE PRECISION      SOLAR1 (          NCASES )
      DOUBLE PRECISION      EMISS1 (          NCASES )

      DOUBLE PRECISION      TRGEP2 (          NCASES )
      DOUBLE PRECISION      SRFVE2 ( 3,       NCASES )
      DOUBLE PRECISION      PHASE2 (          NCASES )
      DOUBLE PRECISION      INCDN2 (          NCASES )
      DOUBLE PRECISION      EMISS2 (          NCASES )

      DOUBLE PRECISION      LS     (          NCASES )

      DOUBLE PRECISION      RECTA1 ( 3,       NCASES )

      DOUBLE PRECISION      PHASE3 (          NCASES )

      DOUBLE PRECISION      RECTAN ( 3 )

      DOUBLE PRECISION      LON    (          NCASES )
      DOUBLE PRECISION      LAT    (          NCASES )
      DOUBLE PRECISION      ALT    (          NCASES )

      INTEGER               B3CODE

      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      LT

      DOUBLE PRECISION      SPOIN3 ( 3,       NCASES )
      DOUBLE PRECISION      TRGEP3 (          NCASES )
      DOUBLE PRECISION      SRFVE3 ( 3,       NCASES )
      LOGICAL               FOUND3 (          NCASES )

      DOUBLE PRECISION      OBSSTA ( 6 )

      DOUBLE PRECISION      STATE1 ( 6,       NCASES )
      DOUBLE PRECISION      LT1    (          NCASES )

      DOUBLE PRECISION      STATE2 ( 6,       NCASES )
      DOUBLE PRECISION      LT2    (          NCASES )

      DOUBLE PRECISION      STATE3 ( 6,       NCASES )
      DOUBLE PRECISION      LT3    (          NCASES )

      DOUBLE PRECISION      POS4   ( 3,       NCASES )
      DOUBLE PRECISION      LT4    (          NCASES )

      DOUBLE PRECISION      SPOIN4 ( 3,       NCASES )
      DOUBLE PRECISION      DIST4  (          NCASES )
      DOUBLE PRECISION      TRGEP4 (          NCASES )
      DOUBLE PRECISION      OBSPO4 ( 3,       NCASES )
      LOGICAL               FOUND4 (          NCASES )

      DOUBLE PRECISION      SPOIN5 ( 3,       NCASES )
      DOUBLE PRECISION      TRGEP5 (          NCASES )
      DOUBLE PRECISION      SRFVE5 ( 3,       NCASES )

      DOUBLE PRECISION      SPOIN6 ( 3,       NCASES )
      DOUBLE PRECISION      ALT6   (          NCASES )

      DOUBLE PRECISION      SPOIN7 ( 3,       NCASES )
      DOUBLE PRECISION      TRGEP7 (          NCASES )
      DOUBLE PRECISION      SRFVE7 ( 3,       NCASES )

      DOUBLE PRECISION      SPOIN8 ( 3,       NCASES )

      DOUBLE PRECISION      ISTATE ( 6 )
      DOUBLE PRECISION      OSTATE ( 6,       NCASES )

      DOUBLE PRECISION      BSIGHT1( 3 )
      DOUBLE PRECISION      BSIGHT2( 3 )
      DOUBLE PRECISION      BOUNDS1( 3 )
      DOUBLE PRECISION      BOUNDS2( 3 )

      INTEGER               NBND   (          NCASES )

      CHARACTER*(MAXL)      FOVSHP ( NCASES )
      CHARACTER*(MAXL)      FOVFRM ( NCASES )

      DOUBLE PRECISION      FOVBS  ( 3,       NCASES )
      DOUBLE PRECISION      FOVBND ( 3,       NCASES )

!
!     Saved variables
!
      SAVE                  BSIGHT1
      SAVE                  BSIGHT2
      SAVE                  BOUNDS1
      SAVE                  BOUNDS2

!
!     Initial values
!
      DATA                  BOUNDS1 /  1.0D0,  1.0D0,  1.0D0 /
      DATA                  BOUNDS2 / -1.0D0, -1.0D0, -1.0D0 /
      DATA                  BSIGHT1 /  0.0D0,  0.0D0,  1.0D0 /
      DATA                  BSIGHT2 /  0.0D0,  0.0D0, -1.0D0 /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZBODS2C' )


!
!     Check ZZBODS2C update after counter set to user value.
!
!     Call ZZBODS2C twice, first to make sure that there is an
!     update, second that there is not update.
!
      CALL TCASE ( 'ZZBODS2C update after initial counter setting.' )

      CALL ZZCTRUIN ( USRCTR )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      TARG   = 'SUN'
      TARGID = -1
      FOUND  = .FALSE.
      SVTARG = 'SUN'
      SVTGID = -1
      SVFND  = .TRUE.

      CALL ZZBODS2C ( USRCTR, SVTARG, SVTGID, SVFND, &
                      TARG, TARGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'USRCTR', USRCTR(1), '!=', SAVCTR(1), 0, OK )
      CALL CHCKSC ( 'SVTARG', SVTARG, '=', 'SUN',            OK )
      CALL CHCKSI ( 'SVTGID', SVTGID, '=', 10,     0,        OK )
      CALL CHCKSL ( 'SVFND',  SVFND,       .TRUE.,           OK )
      CALL CHCKSC ( 'TARG',   TARG,   '=', 'SUN',            OK )
      CALL CHCKSI ( 'TARGID', TARGID, '=', 10,     0,        OK )
      CALL CHCKSL ( 'FOUND',  FOUND,       .TRUE.,           OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      CALL ZZBODS2C ( USRCTR, SVTARG, SVTGID, SVFND, &
                      TARG, TARGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SVTARG', SVTARG, '=', 'SUN',            OK )
      CALL CHCKSI ( 'SVTGID', SVTGID, '=', 10,     0,        OK )
      CALL CHCKSL ( 'SVFND',  SVFND,       .TRUE.,           OK )
      CALL CHCKSC ( 'TARG',   TARG,   '=', 'SUN',            OK )
      CALL CHCKSI ( 'TARGID', TARGID, '=', 10,     0,        OK )
      CALL CHCKSL ( 'FOUND',  FOUND,       .TRUE.,           OK )

!
!     Check ZZBODS2C update after BODDEF mapping update.
!
!     Call ZZBODS2C twice, first to make sure that there is an
!     update, second that there is not update.
!
      CALL TCASE ( 'ZZBODS2C update after BODDEF.' )

      CALL BODDEF ( 'SUN', 20 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      TARG   = 'SUN'
      TARGID = -1
      FOUND  = .FALSE.
      SVTARG = 'SUN'
      SVTGID = 10
      SVFND  = .TRUE.

      CALL ZZBODS2C ( USRCTR, SVTARG, SVTGID, SVFND, &
                      TARG, TARGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'USRCTR', USRCTR(1), '!=', SAVCTR(1), 0, OK )
      CALL CHCKSC ( 'SVTARG', SVTARG, '=', 'SUN',            OK )
      CALL CHCKSI ( 'SVTGID', SVTGID, '=', 20,     0,        OK )
      CALL CHCKSL ( 'SVFND',  SVFND,       .TRUE.,           OK )
      CALL CHCKSC ( 'TARG',   TARG,   '=', 'SUN',            OK )
      CALL CHCKSI ( 'TARGID', TARGID, '=', 20,     0,        OK )
      CALL CHCKSL ( 'FOUND',  FOUND,       .TRUE.,           OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      CALL ZZBODS2C ( USRCTR, SVTARG, SVTGID, SVFND, &
                      TARG, TARGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SVTARG', SVTARG, '=', 'SUN',            OK )
      CALL CHCKSI ( 'SVTGID', SVTGID, '=', 20,     0,        OK )
      CALL CHCKSL ( 'SVFND',  SVFND,       .TRUE.,           OK )
      CALL CHCKSC ( 'TARG',   TARG,   '=', 'SUN',            OK )
      CALL CHCKSI ( 'TARGID', TARGID, '=', 20,     0,        OK )
      CALL CHCKSL ( 'FOUND',  FOUND,       .TRUE.,           OK )

!
!     Check ZZBODS2C update after POOL mapping update.
!
!     Call ZZBODS2C twice, first to make sure that there is an
!     update, second that there is not update.
!
      CALL TCASE ( 'ZZBODS2C update after POOL mapping change.' )

      BUFFER( 1 ) = 'NAIF_BODY_CODE += 30'
      BUFFER( 2 ) = 'NAIF_BODY_NAME += ''SUN'''

      CALL LMPOOL ( BUFFER, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      TARG   = 'SUN'
      TARGID = -1
      FOUND  = .FALSE.
      SVTARG = 'SUN'
      SVTGID = 10
      SVFND  = .TRUE.

      CALL ZZBODS2C ( USRCTR, SVTARG, SVTGID, SVFND, &
                      TARG, TARGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'USRCTR', USRCTR(1), '!=', SAVCTR(1), 0, OK )
      CALL CHCKSC ( 'SVTARG', SVTARG, '=', 'SUN',            OK )
      CALL CHCKSI ( 'SVTGID', SVTGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'SVFND',  SVFND,       .TRUE.,           OK )
      CALL CHCKSC ( 'TARG',   TARG,   '=', 'SUN',            OK )
      CALL CHCKSI ( 'TARGID', TARGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'FOUND',  FOUND,       .TRUE.,           OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      CALL ZZBODS2C ( USRCTR, SVTARG, SVTGID, SVFND, &
                      TARG, TARGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SVTARG', SVTARG, '=', 'SUN',            OK )
      CALL CHCKSI ( 'SVTGID', SVTGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'SVFND',  SVFND,       .TRUE.,           OK )
      CALL CHCKSC ( 'TARG',   TARG,   '=', 'SUN',            OK )
      CALL CHCKSI ( 'TARGID', TARGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'FOUND',  FOUND,       .TRUE.,           OK )

!
!     Check ZZBODS2C update for input saved flag set to .FALSE.
!
!     Call ZZBODS2C twice, first to make sure that there is an
!     update, second that there is not update.
!
      CALL TCASE ( 'ZZBODS2C update after .FALSE. saved flag.' )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      TARG   = 'SUN'
      TARGID = -1
      FOUND  = .FALSE.
      SVTARG = 'SUN'
      SVTGID = -1
      SVFND  = .FALSE.

      CALL ZZBODS2C ( USRCTR, SVTARG, SVTGID, SVFND, &
                      TARG, TARGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SVTARG', SVTARG, '=', 'SUN',            OK )
      CALL CHCKSI ( 'SVTGID', SVTGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'SVFND',  SVFND,       .TRUE.,           OK )
      CALL CHCKSC ( 'TARG',   TARG,   '=', 'SUN',            OK )
      CALL CHCKSI ( 'TARGID', TARGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'FOUND',  FOUND,       .TRUE.,           OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      CALL ZZBODS2C ( USRCTR, SVTARG, SVTGID, SVFND, &
                      TARG, TARGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SVTARG', SVTARG, '=', 'SUN',            OK )
      CALL CHCKSI ( 'SVTGID', SVTGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'SVFND',  SVFND,       .TRUE.,           OK )
      CALL CHCKSC ( 'TARG',   TARG,   '=', 'SUN',            OK )
      CALL CHCKSI ( 'TARGID', TARGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'FOUND',  FOUND,       .TRUE.,           OK )

!
!     Check ZZBODS2C update for different input name.
!
!     Call ZZBODS2C twice, first to make sure that there is an
!     update, second that there is not update.
!
      CALL TCASE ( 'ZZBODS2C update for different input name.' )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      TARG   = 'sun'
      TARGID = -1
      FOUND  = .FALSE.
      SVTARG = 'SUN'
      SVTGID = -1
      SVFND  = .TRUE.

      CALL ZZBODS2C ( USRCTR, SVTARG, SVTGID, SVFND, &
                      TARG, TARGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SVTARG', SVTARG, '=', 'sun',            OK )
      CALL CHCKSI ( 'SVTGID', SVTGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'SVFND',  SVFND,       .TRUE.,           OK )
      CALL CHCKSC ( 'TARG',   TARG,   '=', 'sun',            OK )
      CALL CHCKSI ( 'TARGID', TARGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'FOUND',  FOUND,       .TRUE.,           OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      CALL ZZBODS2C ( USRCTR, SVTARG, SVTGID, SVFND, &
                      TARG, TARGID, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SVTARG', SVTARG, '=', 'sun',            OK )
      CALL CHCKSI ( 'SVTGID', SVTGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'SVFND',  SVFND,       .TRUE.,           OK )
      CALL CHCKSC ( 'TARG',   TARG,   '=', 'sun',            OK )
      CALL CHCKSI ( 'TARGID', TARGID, '=', 30,     0,        OK )
      CALL CHCKSL ( 'FOUND',  FOUND,       .TRUE.,           OK )

!
!     Clear POOL, reset BODTRN, and load some test data.
!
      CALL CLPOOL ( )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZBODRST ( )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KILFIL ( 'spkfun.bsp' )
      CALL KILFIL ( 'spkfun.ker' )

      CALL TSTSPK ( 'spkfun.bsp', .TRUE.,  HANDLE )
      CALL TSTPCK ( 'spkfun.ker', .TRUE., .FALSE. )

!
!     Test all routines that call ZZBODS2C. Do it by computing two
!     bench mark sets of data (at indexes 1 and 2), each for a unique,
!     non-overlapping set of bodies, then setting two unique sets of
!     body aliases and computing the same sets of data for them (at
!     indexes 3 and 4), and checking each "alias" set against its
!     benchmark. It saved mapping was updated correctly set 1 should
!     match set 3 and set 2 should match set 4.
!
!     In addition to setting up all inputs we will call T_CTRBEQF to
!     adjust ZZBODS2C and ZZNAMFRM counters to the same value before
!     calling each of the routine that make use of saved values and
!     counters. This will help exposing any cases when the same counter
!     is erroneously used in more than one place.
!
      DO I = 1, 4

         IF      ( I .EQ. 1 ) THEN

            BODY1 = 'MERCURY'
            BODY2 = 'VENUS'
            BODY3 = 'EARTH'

!
!           Set up FOV keywords required by GETFVN test case
!
            CALL PCPOOL ( 'INS199_FOV_FRAME',    1, 'IAU_MERCURY' )
            CALL PCPOOL ( 'INS199_FOV_SHAPE',    1, 'CIRCLE' )
            CALL PDPOOL ( 'INS199_BORESIGHT',    3, BSIGHT1  )
            CALL PDPOOL ( 'INS199_FOV_BOUNDARY', 3, BOUNDS1  )

            CI = 1

            TITLE = 'Check 1st bench data set against itself'

         ELSE IF ( I .EQ. 2 ) THEN

            BODY1 = 'MARS'
            BODY2 = 'JUPITER'
            BODY3 = 'SATURN'

!
!           Set up FOV keywords required by GETFVN test case
!
            CALL PCPOOL ( 'INS499_FOV_FRAME',    1, 'IAU_MARS' )
            CALL PCPOOL ( 'INS499_FOV_SHAPE',    1, 'CIRCLE' )
            CALL PDPOOL ( 'INS499_BORESIGHT',    3, BSIGHT2  )
            CALL PDPOOL ( 'INS499_FOV_BOUNDARY', 3, BOUNDS2  )

            CI = 2

            TITLE = 'Check 2nd bench data set against itself'

         ELSE IF ( I .EQ. 3 ) THEN

            BODY1 = 'BODY1'
            BODY2 = 'BODY2'
            BODY3 = 'BODY3'

            CI = 1

            BUFFER( 1 ) = 'NAIF_BODY_CODE += 199'
            BUFFER( 2 ) = 'NAIF_BODY_NAME += ''BODY1'''

            BUFFER( 3 ) = 'NAIF_BODY_CODE += 299'
            BUFFER( 4 ) = 'NAIF_BODY_NAME += ''BODY2'''

            BUFFER( 5 ) = 'NAIF_BODY_CODE += 399'
            BUFFER( 6 ) = 'NAIF_BODY_NAME += ''BODY3'''

            CALL LMPOOL ( BUFFER, 6 )

            TITLE = 'Check 1st mapped data set against 1st benchmark'

         ELSE IF ( I .EQ. 4 ) THEN

            BODY1 = 'BODY1'
            BODY2 = 'BODY2'
            BODY3 = 'BODY3'

            CI = 2

            BUFFER( 1 ) = 'NAIF_BODY_CODE += 499'
            BUFFER( 2 ) = 'NAIF_BODY_NAME += ''BODY1'''

            BUFFER( 3 ) = 'NAIF_BODY_CODE += 599'
            BUFFER( 4 ) = 'NAIF_BODY_NAME += ''BODY2'''

            BUFFER( 5 ) = 'NAIF_BODY_CODE += 699'
            BUFFER( 6 ) = 'NAIF_BODY_NAME += ''BODY3'''

            CALL LMPOOL ( BUFFER, 6 )

            TITLE = 'Check 2nd mapped data set against 2nd benchmark'

         END IF

!
!        Declare test case.
!
         CALL TCASE ( TITLE )


!
!        Call all routines that make use of ZZBODS2C. Set arbitrary
!        input time.
!
         ET = 1.0D7

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL BODVRD ( BODY1, 'RADII', 3,  N(I), RADII(1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSI ( 'BODVRD N', &
                       N(I),       '=', N(CI),          0,    OK )
         CALL CHCKAD ( 'BODVRD RADII', &
                       RADII(1,I), '=', RADII(1,CI), 3, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         F = ( RADII(1,I) - RADII(3,I) ) / RADII(1,I)

         CALL T_CTRBEQF ()

         CALL DPGRDR ( BODY1, RADII(1,I), RADII(2,I), RADII(3,I), &
                       RADII(1,I), F, JACOB1(1,1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'DPGRDR JACOBI', &
                       JACOB1(1,1,I), '=', JACOB1(1,1,CI), 9, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL DRDPGR ( BODY1, 0.5D0, 0.5D0, 1.D0, &
                       RADII(1,I), F, JACOB2(1,1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'DRDPGR JACOBI', &
                       JACOB2(1,1,I), '=', JACOB2(1,1,CI), 9, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL BODN2C ( BODY1, B1CODE, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'BODN2C FOUND', FOUND, .TRUE.,  OK )

         CALL CIDFRM ( B1CODE, FRCODE, FIXREF, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'CIDFRM FOUND', FOUND, .TRUE.,  OK )

         CALL T_CTRBEQF ()

         CALL EDTERM ( 'UMBRAL', BODY2, BODY1, ET, &
                       FIXREF, 'NONE', BODY3, NPTS, &
                       TRGEP1(I), OBSPO1(1,I), TRMPT1(1,1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'EDTERM TRGEPC', &
                       TRGEP1(I), '=', TRGEP1(CI), 0.D0, OK )
         CALL CHCKAD ( 'EDTERM OBSPOS', &
                       OBSPO1(1,I), '=', OBSPO1(1,CI), 3, 0.D0, OK )
         CALL CHCKAD ( 'EDTERM JACOBI', &
                TRMPT1(1,1,I), '=', TRMPT1(1,1,CI), 3*NPTS, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL VPACK ( RADII(1,I), 0.D0, 0.D0, SPOINT )

         CALL T_CTRBEQF ()

         CALL ILLUM ( BODY1, ET, 'NONE', BODY3, &
                      SPOINT,  PHASE1(I),  SOLAR1(I), EMISS1(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ILLUM PHASE', &
                       PHASE1(I), '=', PHASE1(CI), 0.D0, OK )
         CALL CHCKSD ( 'ILLUM SOLAR', &
                       SOLAR1(I), '=', SOLAR1(CI), 0.D0, OK )
         CALL CHCKSD ( 'ILLUM EMISSN', &
                       EMISS1(I), '=', EMISS1(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL ILLUMG ( 'Ellipsoid', BODY1, BODY2,  ET, &
                       FIXREF, 'NONE', BODY3, SPOINT, &
                       TRGEP2(I), SRFVE2(1,I), PHASE2(I), &
                       INCDN2(I), EMISS2(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ILLUMG TRGEPC', &
                       TRGEP2(I), '=', TRGEP2(CI), 0.D0, OK )
         CALL CHCKAD ( 'ILLUMG SRFVEC', &
                       SRFVE2(1,I), '=', SRFVE2(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'ILLUMG PHASE', &
                       PHASE2(I), '=', PHASE2(CI), 0.D0, OK )
         CALL CHCKSD ( 'ILLUMG INCDNC', &
                       INCDN2(I), '=', INCDN2(CI), 0.D0, OK )
         CALL CHCKSD ( 'ILLUMG EMISSN', &
                       EMISS2(I), '=', EMISS2(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         LS(I) = LSPCN ( BODY1, ET, 'NONE' )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'LSPCN', &
                       LS(I), '=', LS(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL PGRREC ( BODY1, 0.5D0, 0.5D0, 1.D0, &
                       RADII(1,I), F, RECTA1(1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'PGRREC RECTAN', &
                       RECTA1(1,I), '=', RECTA1(1,CI), 3, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         PHASE3(I) = PHASEQ ( ET, BODY1, BODY2, BODY3, 'NONE' )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'PHASEQ', &
                       PHASE3(I), '=', PHASE3(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL VPACK  ( RADII(1,I), RADII(2,I), RADII(3,I), RECTAN )

         CALL T_CTRBEQF ()

         CALL RECPGR ( BODY1, RECTAN, RADII(1,I), F, &
                       LON(I), LAT(I), ALT(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'RECPGR LON', &
                       LON(I), '=', LON(CI), 0.D0, OK )
         CALL CHCKSD ( 'RECPGR LAT', &
                       LAT(I), '=', LAT(CI), 0.D0, OK )
         CALL CHCKSD ( 'RECPGR ALT', &
                       ALT(I), '=', ALT(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL BODN2C ( BODY3, B3CODE, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'BODN2C FOUND', FOUND, .TRUE.,  OK )

         CALL SPKEZP ( B1CODE, ET, 'J2000', 'NONE', B3CODE, DVEC, LT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CTRBEQF ()

         CALL SINCPT ( 'Ellipsoid', BODY1, ET, FIXREF, &
                       'NONE', BODY3, 'J2000', DVEC, &
                       SPOIN3(1,I), TRGEP3(I), SRFVE3(1,I), FOUND3(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SINCPT SPOINT', &
                       SPOIN3(1,I), '=', SPOIN3(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SINCPT TRGEPC', &
                       TRGEP3(I), '=', TRGEP3(CI), 0.D0, OK )
         CALL CHCKAD ( 'SINCPT SRFVEC', &
                       SRFVE3(1,I), '=', SRFVE3(1,CI), 3, 0.D0, OK )
         CALL CHCKSL ( 'SINCPT FOUND', &
                       FOUND3(I), FOUND3(CI), OK )

!
!        -----------------------------------------------------------
!
         CALL CLEARD ( 6, OBSSTA )

         CALL T_CTRBEQF ()

         CALL SPKCVO ( BODY1, ET, 'J2000', 'OBSERVER', 'NONE', &
                       OBSSTA, ET, BODY3, 'J2000', &
                       STATE1(1,I), LT1(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKCVO STATE', &
                       STATE1(1,I), '=', STATE1(1,CI), 6, 0.D0, OK )
         CALL CHCKSD ( 'SPKCVO LT', &
                       LT1(I), '=', LT1(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SPKCVT ( OBSSTA, ET, BODY1, FIXREF, ET, &
                       'J2000', 'OBSERVER', 'NONE', BODY3, &
                       STATE2(1,I), LT2(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKCVT STATE', &
                       STATE2(1,I), '=', STATE2(1,CI), 6, 0.D0, OK )
         CALL CHCKSD ( 'SPKCVT LT', &
                       LT2(I), '=', LT2(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SPKEZR ( BODY1, ET, 'J2000', 'NONE', BODY3, &
                       STATE3(1,I), LT3(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKEZR STATE', &
                       STATE3(1,I), '=', STATE3(1,CI), 6, 0.D0, OK )
         CALL CHCKSD ( 'SPKEZR LT', &
                       LT3(I), '=', LT3(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SPKPOS ( BODY1, ET, 'J2000', 'NONE', BODY3, &
                       POS4(1,I), LT4(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKPOS POS', &
                       POS4(1,I), '=', POS4(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SPKPOS LT', &
                       LT4(I), '=', LT4(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SRFXPT ( 'Ellipsoid',  BODY1, ET, 'NONE', &
                       BODY3, 'J2000', DVEC, SPOIN4(1,I), &
                       DIST4(I), TRGEP4(I), OBSPO4(1,I), FOUND4(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SRFXPT SPOINT', &
                       SPOIN4(1,I), '=', SPOIN4(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SRFXPT DIST', &
                       DIST4(I), '=', DIST4(CI), 0.D0, OK )
         CALL CHCKSD ( 'SRFXPT TRGEPC', &
                       TRGEP4(I), '=', TRGEP4(CI), 0.D0, OK )
         CALL CHCKAD ( 'SRFXPT OBSPOS', &
                       OBSPO4(1,I), '=', OBSPO4(1,CI), 3, 0.D0, OK )
         CALL CHCKSL ( 'SRFXPT FOUND', &
                       FOUND4(I), FOUND4(CI), OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SUBPNT ( 'Near point: ellipsoid', BODY1, ET, FIXREF, &
                       'NONE', BODY3, &
                       SPOIN5(1,I), TRGEP5(I), SRFVE5(1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SUBPNT SPOINT', &
                       SPOIN5(1,I), '=', SPOIN5(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SUBPNT TRGEPC', &
                       TRGEP5(I), '=', TRGEP5(CI), 0.D0, OK )
         CALL CHCKAD ( 'SUBPNT SRFVEC', &
                       SRFVE5(1,I), '=', SRFVE5(1,CI), 3, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SUBPT ( 'Near point', BODY1, ET, &
                      'NONE', BODY3, SPOIN6(1,I), ALT6(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SUBPT SPOINT', &
                       SPOIN6(1,I), '=', SPOIN6(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SUBPT ALT', &
                       ALT6(I), '=', ALT6(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SUBSLR ( 'Near point: ellipsoid', BODY1, ET, FIXREF, &
                       'NONE', BODY3, &
                       SPOIN7(1,I), TRGEP7(I), SRFVE7(1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SUBSLR SPOINT', &
                       SPOIN7(1,I), '=', SPOIN7(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SUBSLR TRGEPC', &
                       TRGEP7(I), '=', TRGEP7(CI), 0.D0, OK )
         CALL CHCKAD ( 'SUBSLR SRFVEC', &
                       SRFVE7(1,I), '=', SRFVE7(1,CI), 3, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SUBSOL ( 'Near point', BODY1, ET, &
                      'NONE', BODY3, SPOIN8(1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SUBSOL SPOINT', &
                       SPOIN8(1,I), '=', SPOIN8(1,CI), 3, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL VPACK  ( RADII(1,I), RADII(2,I), RADII(3,I), ISTATE(1) )
         CALL VPACK  ( 0.D0,       0.D0,       0.D0,       ISTATE(4) )

         CALL T_CTRBEQF ()

         CALL XFMSTA ( ISTATE, 'RECTANGULAR', 'PLANETOGRAPHIC', BODY1, &
                       OSTATE(1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'XFMSTA OSTATE', &
                       OSTATE(1,I), '=', OSTATE(1,CI), 6, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL GETFVN ( BODY1, ROOM, FOVSHP(I), FOVFRM(I), FOVBS(1,I), &
                       NBND(I), FOVBND(1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSC ( 'GETFVN SHAPE', FOVSHP(I), '=', FOVSHP(CI), OK )
         CALL CHCKSC ( 'GETFVN FRAME', FOVFRM(I), '=', FOVFRM(CI), OK )
         CALL CHCKSI ( 'GETFVN N', NBND(I), '=', NBND(CI), 0, OK )
         CALL CHCKAD ( 'GETFVN BORESIGHT', FOVBS(1,I),  '=', &
                       FOVBS(1,CI), 3, 0.0D0, OK )
         CALL CHCKAD ( 'GETFVN BOUNDS', FOVBND(1,I), '=', &
                       FOVBND(1,CI), 3, 0.0D0, OK )

      END DO


!
!     Clean up.
!
      CALL KILFIL ( 'spkfun.bsp' )
      CALL KILFIL ( 'spkfun.ker' )

!
!     This is good enough for now.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
