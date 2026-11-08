C$Procedure F_DDHFTSIZE ( Handle Manager File Table Size )
 
      SUBROUTINE F_DDHFTSIZE ( OK )
 
C$ Abstract
C
C     Maximum handle manager file table size tests.
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

      INCLUDE               'zzddhman.inc'

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
C     This routine tests maximum handle manager file table size.
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
C
C$ Version
C
C-    TSPICE Version 1.0.0, 02-AUG-2013 (BVS)
C
C
C-&
 
C
C     Local Parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     SPICELIB functions
C
      LOGICAL               EXISTS
      CHARACTER*(WDSIZE)    BEGDAT
      CHARACTER*(WDSIZE)    BEGTXT
      INTEGER               RTRIM

C
C     Local Variables
C
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

C
C     Saved values
C

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_DDHFTSIZE' )

C
C     Set the number of files we will test.
C
      MAXCNT = FTSIZE

C
C     Set the number of files we will load second time.
C
      MAXRLD = MIN( 200, MAXCNT )

C
C     Set flags indicating whether we try to make kernels and whether we
C     try delete kernels.
C
      MAKKER = .TRUE.
      DELKER = .TRUE.

C
C     Set flag to report elapsed time (or not.)
C
C      REPORT = .TRUE.
      REPORT = .FALSE.

      CALL T_ELAPSD ( .FALSE., 'no report on first call', 'total' )

C
C --- Case: ------------------------------------------------------
C
C     Test MAXCNT CKs.
C
C     Make and load CKs.
C
      TCNAME = 'Handle Manager: # CKs'
      CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
      CALL TCASE ( TCNAME )

C
C     Make CK names.
C
      FNMPAT = 'file_ck#.bc'
      DO I = 1, MAXCNT+1
         CALL REPMI  ( FNMPAT, '#', I, FNAME(I) )
      END DO

C
C     Make CK files.
C
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

            CALL CKW03  ( HANDLE, 
     .                    EPOCHS(1),
     .                    EPOCHS(2),
     .                    -999, 
     .                    'J2000',
     .                    .FALSE.,
     .                    FNAME(I),
     .                    2,
     .                    EPOCHS,
     .                    QUATS,
     .                    AVS,
     .                    1,
     .                    EPOCHS(1) )

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CKCLS  ( HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END IF
      
      CALL T_ELAPSD ( REPORT, 'Making CKs', 'running' )

C
C     Load all CKs. Then load the first MAXRLD CKs again to check that
C     they are recongized as known and don't overflow the file table.
C
      DO I = 1, MAXCNT

         CALL FURNSH ( FNAME(I) ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, MAXRLD

         CALL FURNSH ( FNAME(I) ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_ELAPSD ( REPORT, 'Loading CKs', 'running' )

C
C     Read CKs. Check rotations.
C
      DO I = 1, MAXCNT

         CALL CKGP ( -999, DBLE(I), 0.D0, 'J2000', MAT, CLKOUT, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'CKGP FOUND', FOUND, .TRUE., OK )

         CALL M2Q    ( MAT, QUAT1 )

         CALL ROTATE ( DBLE( I * 1.D-3 ), 1, MAT )
         CALL M2Q    ( MAT, QUAT2 )

         CALL CHCKAD ( 'CKGP quaternion',
     .                 QUAT1,
     .                 '~',
     .                 QUAT2,
     .                 4,
     .                 TIGHT,
     .                 OK             )

      END DO

      CALL T_ELAPSD ( REPORT, 'Reading CKs', 'running' )

C
C --- Case: ------------------------------------------------------
C
C     Cause error by loading one more CK.
C
      IF ( MAXCNT .EQ. FTSIZE ) THEN

         TCNAME = 'Handle Manager: #+1 CKs'
         CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
         CALL TCASE ( TCNAME )

         I = MAXCNT + 1

         CALL CKOPN ( FNAME(I), FNAME(I), 0, HANDLE )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFFTFULL)', OK )

      END IF

C
C     Unload all CKs. Doing this one-by-one is EXTREMELY slow!
C
C      DO I = 1, MAXCNT
C
C         WRITE (*,*) 'unloading CK ', I
C
C         CALL UNLOAD ( FNAME(I) ) 
C         CALL CHCKXC ( .FALSE., ' ', OK )
C
C      END DO

      CALL KCLEAR

      IF ( DELKER ) THEN

         DO I = 1, MAXCNT
            
            CALL DELFIL ( FNAME(I) ) 
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END IF

      CALL T_ELAPSD ( REPORT, 'Deleting CKs', 'running' )

C
C --- Case: ------------------------------------------------------
C
C     Test MAXCNT SPKs.
C
C     Make and load SPKs.
C
      TCNAME = 'Handle Manager: # SPKs'
      CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
      CALL TCASE ( TCNAME )

C
C     Make SPK names. Set MK name.
C
      FNMPAT = 'file_spk#.bsp'

      DO I = 1, MAXCNT+1
         CALL REPMI  ( FNMPAT, '#', I, FNAME(I) )
      END DO

      MKNAM    = 'file_spks.tm'

C
C     Make SPK files.
C
      IF ( MAKKER ) THEN

C
C        Set initial MK lines.
C
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

C
C           Make next SPK.
C
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
         
            CALL SPKW09 ( HANDLE, 
     .                    -999, 
     .                    3, 
     .                    'J2000',
     .                    EPOCHS(1),
     .                    EPOCHS(2),
     .                    FNAME(I),
     .                    1,
     .                    2,
     .                    STATES,
     .                    EPOCHS )

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL SPKCLS ( HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make MK lines for this SPK.
C
            NMKTXT = NMKTXT + 1
            MKTXT(NMKTXT) = ''''//FNAME(I)(:RTRIM(FNAME(I)))//''''
            CALL UCASE( MKTXT(NMKTXT), MKTXT(NMKTXT) )
            CALL REPMC ( MKTXT(NMKTXT), 'F', '$F', MKTXT(NMKTXT) )
            CALL REPMC ( MKTXT(NMKTXT), '_', '_$', MKTXT(NMKTXT) )
            CALL REPMC ( MKTXT(NMKTXT), '.', '.$', MKTXT(NMKTXT) )

         END DO

C
C        Make finishing MK lines and write MK.
C
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

C
C     Load all SPKs. Then load the first MAXRLD SPKs again to check that
C     they are recongized as known and don't overflow the file table.
C
      CALL FURNSH ( MKNAM )

C      DO I = 1, MAXCNT
C
C         CALL FURNSH ( FNAME(I) ) 
C         CALL CHCKXC ( .FALSE., ' ', OK )
C
C      END DO

      DO I = 1, MAXRLD

         CALL FURNSH ( FNAME(I) ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_ELAPSD ( REPORT, 'Loading SPKs', 'running' )

C
C     Read SPKs. Check states.
C
      CALL CLEARD( 6, STATE2 ) 
      
      DO I = 1, MAXCNT

         CALL SPKGEO ( -999, DBLE(I), 'J2000', 3, STATE1, LT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         STATE2(1) = DBLE(I)

         CALL CHCKAD ( 'SPKGEO state',
     .                 STATE1,
     .                 '~',
     .                 STATE2,
     .                 6,
     .                 TIGHT,
     .                 OK             )

      END DO

      CALL T_ELAPSD ( REPORT, 'Reading SPKs', 'running' )

C
C --- Case: ------------------------------------------------------
C
C     Cause error by trying to make one more SPK.
C
      IF ( MAXCNT .EQ. FTSIZE ) THEN

         TCNAME = 'Handle Manager: #+1 SPKs'
         CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
         CALL TCASE ( TCNAME )

         I = MAXCNT + 1

         CALL SPKOPN ( FNAME(I), FNAME(I), 0, HANDLE )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFFTFULL)', OK )

      END IF

C
C     Unload all SPKs. Doing this one-by-one is EXTREMELY slow!
C
C      DO I = 1, MAXCNT
C
C         WRITE (*,*) 'unloading SPK ', I
C
C         CALL UNLOAD ( FNAME(I) ) 
C         CALL CHCKXC ( .FALSE., ' ', OK )
C
C      END DO

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

C
C --- Case: ------------------------------------------------------
C
C     Test MAXCNT PCKs.
C
C     Make and load PCKs.
C
      TCNAME = 'Handle Manager: # PCKs'
      CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
      CALL TCASE ( TCNAME )

C
C     Make PCK names.
C
      FNMPAT = 'file_pck#.bpc'
      DO I = 1, MAXCNT+1
         CALL REPMI  ( FNMPAT, '#', I, FNAME(I) )
      END DO

C
C     Make PCK files.
C
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

            CALL PCKW02 (  HANDLE,  
     .                     13000, 
     .                     'J2000',
     .                     EPOCHS(1),
     .                     EPOCHS(2),
     .                     FNAME(I), 
     .                     EPOCHS(2)-EPOCHS(1),
     .                     1,
     .                     1,
     .                     COEFFS,
     .                     EPOCHS(1) )

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL PCKCLS  ( HANDLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END IF
      
      CALL T_ELAPSD ( REPORT, 'Making PCKs', 'running' )

C
C     Load all PCKs. Then load the first MAXRLD PCKs again to check that
C     they are recongized as known and don't overflow the file table.
C
      DO I = 1, MAXCNT

         CALL FURNSH ( FNAME(I) ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      DO I = 1, MAXRLD

         CALL FURNSH ( FNAME(I) ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL T_ELAPSD ( REPORT, 'Loading PCKs', 'running' )

C
C     Read PCKs. Check rotations.
C
      DO I = 1, MAXCNT

         CALL BODMAT ( 13000, DBLE(I), MAT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'BODMAT FOUND', FOUND, .TRUE., OK )

         CALL M2Q    ( MAT, QUAT1 )

         CALL EUL2M  ( DBLE(I)*3.D-3, DBLE(I)*2.D-3, DBLE(I)*1.D-3, 
     .                                                3, 1, 3, MAT )
         CALL M2Q    ( MAT, QUAT2 )

         CALL CHCKAD ( 'BODMAT quaternion',
     .                 QUAT1,
     .                 '~',
     .                 QUAT2,
     .                 4,
     .                 TIGHT,
     .                 OK             )

      END DO

      CALL T_ELAPSD ( REPORT, 'Reading PCKs', 'running' )

C
C --- Case: ------------------------------------------------------
C
C     Cause error by loading one more PCK.
C
      IF ( MAXCNT .EQ. FTSIZE ) THEN

         TCNAME = 'Handle Manager: #+1 PCKs'
         CALL REPMI ( TCNAME, '#', MAXCNT, TCNAME )
         CALL TCASE ( TCNAME )

         I = MAXCNT + 1

         CALL PCKOPN ( FNAME(I), FNAME(I), 0, HANDLE )
         CALL CHCKXC ( .TRUE., 'SPICE(DAFFTFULL)', OK )

      END IF

C
C     Unload all PCKs. Doing this one-by-one is EXTREMELY slow!
C
C      DO I = 1, MAXCNT
C
C         WRITE (*,*) 'unloading PCK ', I
C
C         CALL UNLOAD ( FNAME(I) ) 
C         CALL CHCKXC ( .FALSE., ' ', OK )
C
C      END DO

      CALL KCLEAR

      IF ( DELKER ) THEN

         DO I = 1, MAXCNT
            
            CALL DELFIL ( FNAME(I) ) 
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END IF

      CALL T_ELAPSD ( REPORT, 'Deleting PCKs', 'running' )

C
C     Close out the test family.
C
      CALL T_ELAPSD ( REPORT, 'Total', 'TOTAL' )
      CALL TOSTDO ( ' ' )

      CALL T_SUCCESS ( OK )

      RETURN

      END

