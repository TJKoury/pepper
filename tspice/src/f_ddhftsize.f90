!$Procedure F_DDHFTSIZE ( Handle Manager File Table Size )

      SUBROUTINE F_DDHFTSIZE ( OK )
      use pepper_legacy
      use pepper_inc_zzddhman

! $ Abstract
!
!     Maximum handle manager file table size tests.
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
!     This routine tests maximum handle manager file table size.
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
!     B.V. Semenov     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 02-AUG-2013 (BVS)
!
!
!-&

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     SPICELIB functions
!
      CHARACTER*(WDSIZE)    BEGDAT
      CHARACTER*(WDSIZE)    BEGTXT

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    TCNAME
      CHARACTER*(WDSIZE)    MKTXT  ( FTSIZE + 100 )
      CHARACTER*(WDSIZE)    FNMPAT
      CHARACTER*(WDSIZE)    FNAME  ( FTSIZE + 1 )
      CHARACTER*(WDSIZE)    MKNAM
      INTEGER               I
      DOUBLE PRECISION      EPOCHS (    2 )
      DOUBLE PRECISION      STATES ( 6, 2 )
      DOUBLE PRECISION      STATE1 ( 6 )
      DOUBLE PRECISION      STATE2 ( 6 )
      DOUBLE PRECISION      QUATS  ( 4, 2 )
      DOUBLE PRECISION      AVS    ( 3, 2 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MAT    ( 3, 3 )
      DOUBLE PRECISION      QUAT1  ( 4 )
      DOUBLE PRECISION      QUAT2  ( 4 )
      INTEGER               HANDLE
      DOUBLE PRECISION      CLKOUT
      LOGICAL               FOUND
      INTEGER               MAXCNT
      INTEGER               MAXRLD
      LOGICAL               MAKKER
      LOGICAL               DELKER
      INTEGER               NMKTXT
      INTEGER               UNIT
      DOUBLE PRECISION      COEFFS ( 2, 3 )
      LOGICAL               REPORT

!
!     Saved values
!

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_DDHFTSIZE' )

!
!     Set the number of files we will test.
!
      MAXCNT = FTSIZE

!
!     Set the number of files we will load second time.
!
      MAXRLD = MIN( 200, MAXCNT )

!
!     Set flags indicating whether we try to make kernels and whether we
!     try delete kernels.
!
      MAKKER = .TRUE.
      DELKER = .TRUE.

!
!     Set flag to report elapsed time (or not.)
!
!      REPORT = .TRUE.
      REPORT = .FALSE.

      CALL T_ELAPSD ( .FALSE., 'no report on first call', 'total' )

!
! --- Case: ------------------------------------------------------
!
!     Test MAXCNT CKs.
!
!     Make and load CKs.
!
      TCNAME = 'Handle Manager: # CKs'
      CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
      CALL TCASE ( TCNAME )

!
!     Make CK names.
!
      FNMPAT = 'file_ck#.bc'
      DO I = 1, MAXCNT+1
         CALL REPMI  ( FNMPAT, '#', I, FNAME(I) )
      END DO

!
!     Make CK files.
!
      IF ( MAKKER ) THEN

         CALL CLEARD ( 8, QUATS )
         CALL CLEARD ( 6, AVS )

         DO I = 1, MAXCNT

            IF ( EXISTS(FNAME(I)) ) THEN
               CALL DELFIL(FNAME(I))
            END IF

            CALL CKOPN  ( FNAME(I), ' ', 0, HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            EPOCHS(1)   = DBLE(I)-0.5D0
            EPOCHS(2)   = DBLE(I)+0.5D0

            CALL ROTATE ( DBLE( I * 1.D-3 - 5.D-4 ), 1, MAT )
            CALL M2Q    ( MAT, QUATS(1,1) )

            CALL ROTATE ( DBLE( I * 1.D-3 + 5.D-4 ), 1, MAT )
            CALL M2Q    ( MAT, QUATS(1,2) )

            CALL CKW03  ( HANDLE, &
                          EPOCHS(1), &
                          EPOCHS(2), &
                          -999, &
                          'J2000', &
                          .FALSE., &
                          FNAME(I), &
                          2, &
                          EPOCHS, &
                          QUATS, &
                          AVS, &
                          1, &
                          EPOCHS(1) )

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CKCLS  ( HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END IF

      CALL T_ELAPSD ( REPORT, 'Making CKs', 'running' )

!
!     Load all CKs. Then load the first MAXRLD CKs again to check that
!     they are recongized as known and don't overflow the file table.
!
      DO I = 1, MAXCNT

         CALL FURNSH ( FNAME(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, MAXRLD

         CALL FURNSH ( FNAME(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_ELAPSD ( REPORT, 'Loading CKs', 'running' )

!
!     Read CKs. Check rotations.
!
      DO I = 1, MAXCNT

         CALL CKGP ( -999, DBLE(I), 0.D0, 'J2000', MAT, CLKOUT, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'CKGP FOUND', FOUND, .TRUE., OK )

         CALL M2Q    ( MAT, QUAT1 )

         CALL ROTATE ( DBLE( I * 1.D-3 ), 1, MAT )
         CALL M2Q    ( MAT, QUAT2 )

         CALL CHCKAD ( 'CKGP quaternion', &
                       QUAT1, &
                       '~', &
                       QUAT2, &
                       4, &
                       TIGHT, &
                       OK             )

      END DO

      CALL T_ELAPSD ( REPORT, 'Reading CKs', 'running' )

!
! --- Case: ------------------------------------------------------
!
!     Cause error by loading one more CK.
!
      IF ( MAXCNT .EQ. FTSIZE ) THEN

         TCNAME = 'Handle Manager: #+1 CKs (PEPPER: no DAFFTFULL)'
         CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
         CALL TCASE ( TCNAME )

         I = MAXCNT + 1

!
!        PEPPER: the DAF and handle manager file tables grow (SPICELIB
!        signaled SPICE(DAFFTFULL) here): the file opens. Close and
!        delete it.
!
         IF ( EXISTS(FNAME(I)) ) THEN
            CALL DELFIL ( FNAME(I) )
         END IF

         CALL CKOPN ( FNAME(I), FNAME(I), 0, HANDLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( FNAME(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

!
!     Unload all CKs. Doing this one-by-one is EXTREMELY slow!
!
!      DO I = 1, MAXCNT
!
!         WRITE (*,*) 'unloading CK ', I
!
!         CALL UNLOAD ( FNAME(I) )
!         CALL CHCKXC ( .FALSE., ' ', OK )
!
!      END DO

      CALL KCLEAR

      IF ( DELKER ) THEN

         DO I = 1, MAXCNT

            CALL DELFIL ( FNAME(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END IF

      CALL T_ELAPSD ( REPORT, 'Deleting CKs', 'running' )

!
! --- Case: ------------------------------------------------------
!
!     Test MAXCNT SPKs.
!
!     Make and load SPKs.
!
      TCNAME = 'Handle Manager: # SPKs'
      CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
      CALL TCASE ( TCNAME )

!
!     Make SPK names. Set MK name.
!
      FNMPAT = 'file_spk#.bsp'

      DO I = 1, MAXCNT+1
         CALL REPMI  ( FNMPAT, '#', I, FNAME(I) )
      END DO

      MKNAM    = 'file_spks.tm'

!
!     Make SPK files.
!
      IF ( MAKKER ) THEN

!
!        Set initial MK lines.
!
         MKTXT(1)  = 'KPL/MK'
         MKTXT(2)  = BEGDAT()
         MKTXT(3)  = 'PATH_SYMBOLS    = ('
         MKTXT(4)  = '''FILE'''
         MKTXT(5)  = '''SPK'''
         MKTXT(6)  = '''BSP'''
         MKTXT(7)  = ')'
         MKTXT(8)  = 'PATH_VALUES    = ('
         MKTXT(9)  = '''file'''
         MKTXT(10) = '''spk'''
         MKTXT(11) = '''bsp'''
         MKTXT(12) = ')'
         MKTXT(13) = 'KERNELS_TO_LOAD = ('
         NMKTXT    = 13

         CALL CLEARD ( 12, STATES )

         DO I = 1, MAXCNT

!
!           Make next SPK.
!
            IF ( EXISTS( FNAME(I) ) ) THEN
               CALL DELFIL ( FNAME(I) )
               CALL CHCKXC ( .FALSE., ' ', OK )
            END IF

            CALL SPKOPN ( FNAME(I), ' ', 0, HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            EPOCHS(1)   = DBLE(I)-0.5D0
            EPOCHS(2)   = DBLE(I)+0.5D0

            STATES(1,1) = EPOCHS(1)
            STATES(1,2) = EPOCHS(2)

            CALL SPKW09 ( HANDLE, &
                          -999, &
                          3, &
                          'J2000', &
                          EPOCHS(1), &
                          EPOCHS(2), &
                          FNAME(I), &
                          1, &
                          2, &
                          STATES, &
                          EPOCHS )

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL SPKCLS ( HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Make MK lines for this SPK.
!
            NMKTXT = NMKTXT + 1
            MKTXT(NMKTXT) = ''''//FNAME(I)(:RTRIM(FNAME(I)))//''''
            CALL UCASE( MKTXT(NMKTXT), MKTXT(NMKTXT) )
            CALL REPMC ( MKTXT(NMKTXT), 'F', '$F', MKTXT(NMKTXT) )
            CALL REPMC ( MKTXT(NMKTXT), '_', '_$', MKTXT(NMKTXT) )
            CALL REPMC ( MKTXT(NMKTXT), '.', '.$', MKTXT(NMKTXT) )

         END DO

!
!        Make finishing MK lines and write MK.
!
         MKTXT(NMKTXT+1) = ')'
         MKTXT(NMKTXT+2) = BEGTXT()
         MKTXT(NMKTXT+3) = 'End of MK file.'
         NMKTXT   = NMKTXT + 3

         IF ( EXISTS(MKNAM) ) THEN
            CALL DELFIL(MKNAM)
         END IF

         CALL TXTOPN ( MKNAM, UNIT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL WRITLA ( NMKTXT, MKTXT, UNIT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CLOSE ( UNIT )

      END IF

      CALL T_ELAPSD ( REPORT, 'Making SPKs', 'running' )

!
!     Load all SPKs. Then load the first MAXRLD SPKs again to check that
!     they are recongized as known and don't overflow the file table.
!
      CALL FURNSH ( MKNAM )

!      DO I = 1, MAXCNT
!
!         CALL FURNSH ( FNAME(I) )
!         CALL CHCKXC ( .FALSE., ' ', OK )
!
!      END DO

      DO I = 1, MAXRLD

         CALL FURNSH ( FNAME(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_ELAPSD ( REPORT, 'Loading SPKs', 'running' )

!
!     Read SPKs. Check states.
!
      CALL CLEARD( 6, STATE2 )

      DO I = 1, MAXCNT

         CALL SPKGEO ( -999, DBLE(I), 'J2000', 3, STATE1, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         STATE2(1) = DBLE(I)

         CALL CHCKAD ( 'SPKGEO state', &
                       STATE1, &
                       '~', &
                       STATE2, &
                       6, &
                       TIGHT, &
                       OK             )

      END DO

      CALL T_ELAPSD ( REPORT, 'Reading SPKs', 'running' )

!
! --- Case: ------------------------------------------------------
!
!     Cause error by trying to make one more SPK.
!
      IF ( MAXCNT .EQ. FTSIZE ) THEN

         TCNAME = 'Handle Manager: #+1 SPKs (PEPPER: no DAFFTFULL)'
         CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
         CALL TCASE ( TCNAME )

         I = MAXCNT + 1

!
!        PEPPER: the DAF and handle manager file tables grow (SPICELIB
!        signaled SPICE(DAFFTFULL) here): the file opens. Close and
!        delete it.
!
         IF ( EXISTS(FNAME(I)) ) THEN
            CALL DELFIL ( FNAME(I) )
         END IF

         CALL SPKOPN ( FNAME(I), FNAME(I), 0, HANDLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( FNAME(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

!
!     Unload all SPKs. Doing this one-by-one is EXTREMELY slow!
!
!      DO I = 1, MAXCNT
!
!         WRITE (*,*) 'unloading SPK ', I
!
!         CALL UNLOAD ( FNAME(I) )
!         CALL CHCKXC ( .FALSE., ' ', OK )
!
!      END DO

      CALL KCLEAR

      IF ( DELKER ) THEN

         DO I = 1, MAXCNT

            CALL DELFIL ( FNAME(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

         CALL DELFIL ( MKNAM )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL T_ELAPSD ( REPORT, 'Deleting SPKs', 'running' )

!
! --- Case: ------------------------------------------------------
!
!     Test MAXCNT PCKs.
!
!     Make and load PCKs.
!
      TCNAME = 'Handle Manager: # PCKs'
      CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
      CALL TCASE ( TCNAME )

!
!     Make PCK names.
!
      FNMPAT = 'file_pck#.bpc'
      DO I = 1, MAXCNT+1
         CALL REPMI  ( FNMPAT, '#', I, FNAME(I) )
      END DO

!
!     Make PCK files.
!
      IF ( MAKKER ) THEN

         CALL CLEARD ( 8, QUATS )
         CALL CLEARD ( 6, AVS )

         DO I = 1, MAXCNT

            IF ( EXISTS( FNAME(I) ) ) THEN
               CALL DELFIL( FNAME(I) )
            END IF

            CALL PCKOPN ( FNAME(I), ' ', 0, HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            EPOCHS(1)   = DBLE(I)-0.5D0
            EPOCHS(2)   = DBLE(I)+0.5D0

            COEFFS(1,1) =  DBLE(I)*1.D-3
            COEFFS(2,1) =  0.D0

            COEFFS(1,2) =  DBLE(I)*2.D-3
            COEFFS(2,2) =  0.D0

            COEFFS(1,3) =  DBLE(I)*3.D-3
            COEFFS(2,3) =  0.D0

            CALL PCKW02 (  HANDLE, &
                           13000, &
                           'J2000', &
                           EPOCHS(1), &
                           EPOCHS(2), &
                           FNAME(I), &
                           EPOCHS(2)-EPOCHS(1), &
                           1, &
                           1, &
                           COEFFS, &
                           EPOCHS(1) )

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL PCKCLS  ( HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END IF

      CALL T_ELAPSD ( REPORT, 'Making PCKs', 'running' )

!
!     Load all PCKs. Then load the first MAXRLD PCKs again to check that
!     they are recongized as known and don't overflow the file table.
!
      DO I = 1, MAXCNT

         CALL FURNSH ( FNAME(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, MAXRLD

         CALL FURNSH ( FNAME(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_ELAPSD ( REPORT, 'Loading PCKs', 'running' )

!
!     Read PCKs. Check rotations.
!
      DO I = 1, MAXCNT

         CALL BODMAT ( 13000, DBLE(I), MAT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'BODMAT FOUND', FOUND, .TRUE., OK )

         CALL M2Q    ( MAT, QUAT1 )

         CALL EUL2M  ( DBLE(I)*3.D-3, DBLE(I)*2.D-3, DBLE(I)*1.D-3, &
                                                      3, 1, 3, MAT )
         CALL M2Q    ( MAT, QUAT2 )

         CALL CHCKAD ( 'BODMAT quaternion', &
                       QUAT1, &
                       '~', &
                       QUAT2, &
                       4, &
                       TIGHT, &
                       OK             )

      END DO

      CALL T_ELAPSD ( REPORT, 'Reading PCKs', 'running' )

!
! --- Case: ------------------------------------------------------
!
!     Cause error by loading one more PCK.
!
      IF ( MAXCNT .EQ. FTSIZE ) THEN

         TCNAME = 'Handle Manager: #+1 PCKs (PEPPER: no DAFFTFULL)'
         CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
         CALL TCASE ( TCNAME )

         I = MAXCNT + 1

!
!        PEPPER: the DAF and handle manager file tables grow (SPICELIB
!        signaled SPICE(DAFFTFULL) here): the file opens. Close and
!        delete it.
!
         IF ( EXISTS(FNAME(I)) ) THEN
            CALL DELFIL ( FNAME(I) )
         END IF

         CALL PCKOPN ( FNAME(I), FNAME(I), 0, HANDLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DAFCLS ( HANDLE )
         CALL DELFIL ( FNAME(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

!
!     Unload all PCKs. Doing this one-by-one is EXTREMELY slow!
!
!      DO I = 1, MAXCNT
!
!         WRITE (*,*) 'unloading PCK ', I
!
!         CALL UNLOAD ( FNAME(I) )
!         CALL CHCKXC ( .FALSE., ' ', OK )
!
!      END DO

      CALL KCLEAR

      IF ( DELKER ) THEN

         DO I = 1, MAXCNT

            CALL DELFIL ( FNAME(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END IF

      CALL T_ELAPSD ( REPORT, 'Deleting PCKs', 'running' )

!
!     Close out the test family.
!
      CALL T_ELAPSD ( REPORT, 'Total', 'TOTAL' )
      CALL TOSTDO ( ' ' )

      CALL T_SUCCESS ( OK )

      RETURN

      END

