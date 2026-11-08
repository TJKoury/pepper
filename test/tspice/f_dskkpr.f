C$Procedure F_DSKKPR ( DSK KEEPER tests )
 
      SUBROUTINE F_DSKKPR ( OK )
 
C$ Abstract
C
C     Exercise entry points of the SPICELIB routine KEEPER, using
C     DSK files.
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
      INCLUDE 'dla.inc'
      INCLUDE 'dskdsc.inc'

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
C     This routine tests entry points of the SPICELIB routine KEEPER,
C     using DSK files. Entry points exercised by this test family are:
C
C        FURNSH
C        UNLOAD
C        KCLEAR
C        KTOTAL
C        KINFO
C        KDATA
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 10-AUG-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Test utility functions
C
      CHARACTER*(32)        BEGDAT
      CHARACTER*(32)        BEGTXT

C
C     Local Parameters
C
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

C 
C     Local Variables
C
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
      
C
C     Saved values
C      
C     Initial values
C
 

C
C     Open the test family.
C
      CALL TOPEN ( 'F_DSKKPR' )
 

C***********************************************************************
C
C     KEEPER entry point tests
C
C***********************************************************************



C
C --- Case: ------------------------------------------------------
C
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
   

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FURNSH test: load a DSK file.' )
 
      CALL FURNSH ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'KTOTAL test: count loaded DSK files.' )

      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', 1, 0, OK )

      CALL KTOTAL ( 'ALL', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (all)', TOTAL, '=', 1, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'UNLOAD test: unload the DSK file.' )
 
      CALL UNLOAD ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of loaded DSKs.
C
      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', 0, 0, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'KCLEAR test: load and then unload the DSK file.' )
 


      CALL FURNSH ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the number of loaded DSKs.
C
      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', 0, 0, OK )


C
C --- Case: ------------------------------------------------------
C
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

C
C     Create meta-kernel.
C
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

C
C     Load meta-kernel.
C
      CALL FURNSH ( META )
      CALL CHCKXC   ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'KTOTAL test (MK): count loaded DSK files.' )

      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', NSMALL, 0, OK )

      CALL KTOTAL ( 'ALL', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (all)', TOTAL, '=', NSMALL+1, 0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'KINFO (MK): check info on loaded DSKs.' )


C
C     Check info on meta-kernel.
C
      CALL KINFO  ( META, FILTYP, SOURCE, HANDLE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,       OK )
      CALL CHCKSC ( 'FILTYP', FILTYP, '=', 'META',  OK )



C
C     Check info on DSKs.
C
      DO I = 1, NSMALL

         CALL KINFO  ( DSKS(I), FILTYP, SOURCE, HANDLE, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,     OK )

         IF ( OK ) THEN

            CALL CHCKSC ( 'FILTYP', FILTYP, '=', 'DSK', OK )
            CALL CHCKSC ( 'SOURCE', SOURCE, '=', META,  OK )

C
C           Check the handle of the Ith DSK.
C
C           Use the handle to extract the surface ID of the 
C           sole segment of the current file. This ID is unique
C           among the loaded DSKs.
C           
            CALL DLABFS ( HANDLE, DLADSC, FND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DSKGD  ( HANDLE, DLADSC, DSKDSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            SURFID = NINT(DSKDSC(SRFIDX))

            CALL CHCKSI ( 'SURFID', SURFID, '=', I, 0, OK )
     .                    

         END IF

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'KDATA (MK): check info on loaded DSKs.' )

 
C
C     Check info on meta-kernel.
C
      CALL KDATA  ( 1, 'META', FNAME, FILTYP, SOURCE, HANDLE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,       OK )
      CALL CHCKSC ( 'FNAME',  FNAME,  '=', META,    OK )
      CALL CHCKSC ( 'FILTYP', FILTYP, '=', 'META',  OK )

C
C     Check info on DSKs.
C
      DO I = 1, NSMALL

         CALL KDATA  ( I, 'DSK', FNAME, FILTYP, SOURCE, HANS(I), FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,     OK )

         IF ( OK ) THEN

            CALL CHCKSC ( 'FILE',   FNAME,  '=', DSKS(I), OK )
            CALL CHCKSC ( 'FILTYP', FILTYP, '=', 'DSK',   OK )
            CALL CHCKSC ( 'SOURCE', SOURCE, '=', META,    OK )

C
C           Check the handle of the Ith DSK.
C
C           Use the handle to extract the surface ID of the 
C           sole segment of the current file. This ID is unique
C           among the loaded DSKs.
C           
            CALL DLABFS ( HANS(I), DLADSC, FND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DSKGD  ( HANS(I), DLADSC, DSKDSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            SURFID = NINT(DSKDSC(SRFIDX))

            CALL CHCKSI ( 'SURFID', SURFID, '=', I, 0, OK )
     .                    
         END IF

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'UNLOAD: unload meta-kernel.' )


      CALL UNLOAD ( META )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', 0, 0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FURNSH: reload meta-kernel.' )


      CALL FURNSH ( META )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C     Tests using the maximum number of loaded DSKs follow.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FURNSH test: load 5000 DSK files.' )


      N = 5000

C
C     Create the DSKs that weren't there already from the
C     meta-kernel test cases.
C
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
     
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'KTOTAL: check loaded DSK count.' )


      CALL KTOTAL ( 'DSK', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (dsk)', TOTAL, '=', NSMALL + N, 0, OK )
      


C
C --- Case: ------------------------------------------------------
C
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

C
C           Check the handle of the Ith DSK.
C
C           Use the handle to extract the surface ID of the 
C           sole segment of the current file. This ID is unique
C           among the loaded DSKs.
C           
            CALL DLABFS ( HANDLE, DLADSC, FND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DSKGD  ( HANDLE, DLADSC, DSKDSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            SURFID = NINT(DSKDSC(SRFIDX))

            CALL CHCKSI ( 'SURFID', SURFID, '=', I, 0, OK )
     .                    

         END IF

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'KCLEAR: unload all 5000 DSKs.' )

C
C     Unload all files.
C
      CALL KCLEAR
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Note: unloading via UNLOAD is still an n**2 process: it's
C     too slow with 5000 files.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'KDATA: check info on loaded DSKs.' )

C
C     Reload DSKs.
C     
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

C
C           Check the handle of the Ith DSK.
C
C           Use the handle to extract the surface ID of the 
C           sole segment of the current file. This ID is unique
C           among the loaded DSKs.
C           
            CALL DLABFS ( HANS(I), DLADSC, FND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DSKGD  ( HANS(I), DLADSC, DSKDSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            SURFID = NINT(DSKDSC(SRFIDX))

            CALL CHCKSI ( 'SURFID', SURFID, '=', I, 0, OK )
     .                    
         END IF

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'KCLEAR/KTOTAL: verify keeper clear operation.' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KTOTAL ( 'ALL', TOTAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'TOTAL (ALL)', TOTAL, '=', 0, 0, OK )

C
C     Clean up.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up.' )

      CALL DELFIL ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N

         CALL DELFIL ( DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO
       
      CALL DELFIL ( META )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


