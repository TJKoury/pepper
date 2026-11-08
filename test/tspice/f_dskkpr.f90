!$Procedure F_DSKKPR ( DSK KEEPER tests )

      SUBROUTINE F_DSKKPR ( OK )

! $ Abstract
!
!     Exercise entry points of the SPICELIB routine KEEPER, using
!     DSK files.
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
      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'

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
!     This routine tests entry points of the SPICELIB routine KEEPER,
!     using DSK files. Entry points exercised by this test family are:
!
!        FURNSH
!        UNLOAD
!        KCLEAR
!        KTOTAL
!        KINFO
!        KDATA
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 10-AUG-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               EXISTS

!
!     Test utility functions
!
      CHARACTER*(32)        BEGDAT
      CHARACTER*(32)        BEGTXT

!
!     Local Parameters
!
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )

      INTEGER               MAXN
      PARAMETER           ( MAXN   = 5000 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               KERSIZ
      PARAMETER           ( KERSIZ = 12 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Local Variables
!
      CHARACTER*(FILSIZ)    DSK
      CHARACTER*(FILSIZ)    DSKS   ( MAXN )
      CHARACTER*(TYPLEN)    FILTYP
      CHARACTER*(FILSIZ)    FNAME
      CHARACTER*(FRNMLN)    FRAME
      CHARACTER*(LNSIZE)    KERTXT ( KERSIZ )
      CHARACTER*(FILSIZ)    META
      CHARACTER*(FILSIZ)    SOURCE

      DOUBLE PRECISION      DSKDSC ( DSKDSZ )

      INTEGER               BODYID
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               HANDLE
      INTEGER               HANS   ( MAXN )
      INTEGER               I
      INTEGER               N
      INTEGER               NSMALL
      INTEGER               SURFID
      INTEGER               TOTAL
      INTEGER               UNIT

      LOGICAL               FND
      LOGICAL               FOUND

!
!     Saved values
!
!     Initial values
!


!
!     Open the test family.
!
      CALL TOPEN ( 'F_DSKKPR' )


!***********************************************************************
!
!     KEEPER entry point tests
!
!***********************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create a DSK file.' )

      BODYID = 499
      SURFID = 1
      FRAME  = 'IAU_MARS'

      DSK    = 'keeper_test_0.bds'

      IF ( EXISTS(DSK) ) THEN

         CALL DELFIL ( DSK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSK )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'FURNSH test: load a DSK file.' )

      CALL FURNSH ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'KTOTAL test: count loaded DSK files.' )

      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', 1, 0, OK )

      CALL KTOTAL ( 'ALL', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (all)', TOTAL, '=', 1, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'UNLOAD test: unload the DSK file.' )

      CALL UNLOAD ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of loaded DSKs.
!
      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', 0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'KCLEAR test: load and then unload the DSK file.' )



      CALL FURNSH ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of loaded DSKs.
!
      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', 0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'FURNSH test: load DSKs via meta-kernel.' )


      KERTXT(1)  = 'KPL/MK'
      KERTXT(2)  = BEGDAT()
      KERTXT(3)  = 'KERNELS_TO_LOAD = ( '
      KERTXT(4)  = ' '
      KERTXT(5)  = '   ''keeper_test_1.bds'''
      KERTXT(6)  = '   ''keeper_test_2.bds'''
      KERTXT(7)  = '   ''keeper_test_3.bds'''
      KERTXT(8)  = '   ''keeper_test_4.bds'''
      KERTXT(9)  = '   ''keeper_test_5.bds'''
      KERTXT(10) = ' '
      KERTXT(11) = ' ) '
      KERTXT(12) = BEGTXT()


      NSMALL = 5

      DO I = 1, NSMALL

         DSKS(I) = 'keeper_test_#.bds'
         CALL REPMI ( DSKS(I), '#', I, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL T_SMLDSK ( BODYID, I, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

!
!     Create meta-kernel.
!
      META = 'keeper_meta.tm'

      IF ( EXISTS(META) ) THEN
         CALL DELFIL( META )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TXTOPN ( META, UNIT )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO I = 1, KERSIZ

         CALL WRITLN ( KERTXT(I), UNIT )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

      CLOSE ( UNIT )

!
!     Load meta-kernel.
!
      CALL FURNSH ( META )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'KTOTAL test (MK): count loaded DSK files.' )

      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', NSMALL, 0, OK )

      CALL KTOTAL ( 'ALL', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (all)', TOTAL, '=', NSMALL+1, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'KINFO (MK): check info on loaded DSKs.' )


!
!     Check info on meta-kernel.
!
      CALL KINFO  ( META, FILTYP, SOURCE, HANDLE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,       OK )
      CALL CHCKSC ( 'FILTYP', FILTYP, '=', 'META',  OK )



!
!     Check info on DSKs.
!
      DO I = 1, NSMALL

         CALL KINFO  ( DSKS(I), FILTYP, SOURCE, HANDLE, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,     OK )

         IF ( OK ) THEN

            CALL CHCKSC ( 'FILTYP', FILTYP, '=', 'DSK', OK )
            CALL CHCKSC ( 'SOURCE', SOURCE, '=', META,  OK )

!
!           Check the handle of the Ith DSK.
!
!           Use the handle to extract the surface ID of the
!           sole segment of the current file. This ID is unique
!           among the loaded DSKs.
!
            CALL DLABFS ( HANDLE, DLADSC, FND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DSKGD  ( HANDLE, DLADSC, DSKDSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            SURFID = NINT(DSKDSC(SRFIDX))

            CALL CHCKSI ( 'SURFID', SURFID, '=', I, 0, OK )


         END IF

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'KDATA (MK): check info on loaded DSKs.' )


!
!     Check info on meta-kernel.
!
      CALL KDATA  ( 1, 'META', FNAME, FILTYP, SOURCE, HANDLE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,       OK )
      CALL CHCKSC ( 'FNAME',  FNAME,  '=', META,    OK )
      CALL CHCKSC ( 'FILTYP', FILTYP, '=', 'META',  OK )

!
!     Check info on DSKs.
!
      DO I = 1, NSMALL

         CALL KDATA  ( I, 'DSK', FNAME, FILTYP, SOURCE, HANS(I), FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,     OK )

         IF ( OK ) THEN

            CALL CHCKSC ( 'FILE',   FNAME,  '=', DSKS(I), OK )
            CALL CHCKSC ( 'FILTYP', FILTYP, '=', 'DSK',   OK )
            CALL CHCKSC ( 'SOURCE', SOURCE, '=', META,    OK )

!
!           Check the handle of the Ith DSK.
!
!           Use the handle to extract the surface ID of the
!           sole segment of the current file. This ID is unique
!           among the loaded DSKs.
!
            CALL DLABFS ( HANS(I), DLADSC, FND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DSKGD  ( HANS(I), DLADSC, DSKDSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            SURFID = NINT(DSKDSC(SRFIDX))

            CALL CHCKSI ( 'SURFID', SURFID, '=', I, 0, OK )

         END IF

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'UNLOAD: unload meta-kernel.' )


      CALL UNLOAD ( META )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'FURNSH: reload meta-kernel.' )


      CALL FURNSH ( META )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
!     Tests using the maximum number of loaded DSKs follow.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'FURNSH test: load 5000 DSK files.' )


      N = 5000

!
!     Create the DSKs that weren't there already from the
!     meta-kernel test cases.
!
      DO I = NSMALL+1, N

         DSKS(I) = 'keeper_test_#.bds'
         CALL REPMI ( DSKS(I), '#', I, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL T_SMLDSK ( BODYID, I, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

      DO I = 1, N

         CALL FURNSH ( DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'KTOTAL: check loaded DSK count.' )


      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', NSMALL + N, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'KINFO: check info on loaded DSKs.' )


      DO I = 1, N

         CALL KINFO  ( DSKS(I), FILTYP, SOURCE, HANDLE, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,     OK )

         IF ( OK ) THEN

            CALL CHCKSC ( 'FILTYP', FILTYP, '=', 'DSK', OK )

            IF ( I .GT. NSMALL ) THEN
               CALL CHCKSC ( 'SOURCE', SOURCE, '=', ' ',   OK )
            END IF

!
!           Check the handle of the Ith DSK.
!
!           Use the handle to extract the surface ID of the
!           sole segment of the current file. This ID is unique
!           among the loaded DSKs.
!
            CALL DLABFS ( HANDLE, DLADSC, FND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DSKGD  ( HANDLE, DLADSC, DSKDSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            SURFID = NINT(DSKDSC(SRFIDX))

            CALL CHCKSI ( 'SURFID', SURFID, '=', I, 0, OK )


         END IF

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'KCLEAR: unload all 5000 DSKs.' )

!
!     Unload all files.
!
      CALL KCLEAR
      CALL CHCKXC   ( .FALSE., ' ', OK )

!
!     Note: unloading via UNLOAD is still an n**2 process: it's
!     too slow with 5000 files.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'KDATA: check info on loaded DSKs.' )

!
!     Reload DSKs.
!
      DO I = 1, N
         CALL FURNSH ( DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )
      END DO

      DO I = 1, N

         CALL KDATA  ( I, 'DSK', FNAME, FILTYP, SOURCE, HANS(I), FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,     OK )

         IF ( OK ) THEN

            CALL CHCKSC ( 'FILE',   FNAME,  '=', DSKS(I), OK )
            CALL CHCKSC ( 'FILTYP', FILTYP, '=', 'DSK',   OK )
            CALL CHCKSC ( 'SOURCE', SOURCE, '=', ' ',     OK )

!
!           Check the handle of the Ith DSK.
!
!           Use the handle to extract the surface ID of the
!           sole segment of the current file. This ID is unique
!           among the loaded DSKs.
!
            CALL DLABFS ( HANS(I), DLADSC, FND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DSKGD  ( HANS(I), DLADSC, DSKDSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            SURFID = NINT(DSKDSC(SRFIDX))

            CALL CHCKSI ( 'SURFID', SURFID, '=', I, 0, OK )

         END IF

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'KCLEAR/KTOTAL: verify keeper clear operation.' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KTOTAL ( 'ALL', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (ALL)', TOTAL, '=', 0, 0, OK )

!
!     Clean up.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up.' )

      CALL DELFIL ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         CALL DELFIL ( DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL DELFIL ( META )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


