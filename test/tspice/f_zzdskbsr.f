C$Procedure F_ZZDSKBSR ( ZZDSKBSR tests )
 
      SUBROUTINE F_ZZDSKBSR ( OK )
 
C$ Abstract
C
C     Exercise entry points of the SPICELIB routine ZZDSKSBR.
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
      INCLUDE 'zzctr.inc'

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
C        ZZDSKBSR (umbrella)
C        ZZDSKLSF
C        ZZDSKUSF
C        ZZDSKBSS
C        ZZDSKSNS
C        ZZDSKCHK
C
C     The routine
C
C        ZZDSKBBL 
C
C     is also exercised by this test family.
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
C-    TSPICE Version 1.0.0, 08-JUL-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EVEN
      LOGICAL               EXISTS
      LOGICAL               ODD
      LOGICAL               ZZDSKBDC
      LOGICAL               ZZDSKSBD

C
C     EXTERNAL functions
C
      EXTERNAL              ZZDSKBDC
      EXTERNAL              ZZDSKNOT
      EXTERNAL              ZZDSKSBD

C
C     ZZDSKBSR parameters
C
      INTEGER               BTSIZE
      PARAMETER           ( BTSIZE = 100 )

      INTEGER               STSIZE
      PARAMETER           ( STSIZE = 10000 )

      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 5000 )



C
C     Local Parameters
C
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )

      INTEGER               MAXN
      PARAMETER           ( MAXN   = FTSIZE + 4 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               KERSIZ
      PARAMETER           ( KERSIZ = 12 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 40 )

C 
C     Local Variables
C
      CHARACTER*(FILSIZ)    BTDSK
      CHARACTER*(FILSIZ)    DSKS   ( MAXN )
      CHARACTER*(FRNMLN)    FRAME
      CHARACTER*(LBLSIZ)    LABEL
      CHARACTER*(FILSIZ)    OVRDSK
      
      DOUBLE PRECISION      DSKDSC ( DSKDSZ )
      DOUBLE PRECISION      XDSKDS ( DSKDSZ )

      INTEGER               BODYID
      INTEGER               BTHAN
      INTEGER               CENTRS ( BTSIZE+1 )
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               DLADSC ( DLADSZ )
      INTEGER               DSKCTR ( CTRSIZ )
      INTEGER               FILENO
      INTEGER               FRAMID
      INTEGER               HANDLE
      INTEGER               HANDLS  ( MAXN )
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NBOD
      INTEGER               NEARTH
      INTEGER               NSAT
      INTEGER               NSEG
      INTEGER               NXTDSC ( DLADSZ )
      INTEGER               OVRHAN
      INTEGER               PRVDSC ( DLADSZ )
      INTEGER               SURFID
      INTEGER               XDLADS ( DLADSZ )

      LOGICAL               FND
      LOGICAL               FOUND
      LOGICAL               RETVAL
      LOGICAL               UPDATE
      
C
C     Saved values
C      
C
C     Save everything in order to avoid stack problems on
C     some platforms.
C
      SAVE


C     Initial values
C
 

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZDSKBSR' )
 

C***********************************************************************
C
C     ZZDSKBSR entry point tests
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKBSR error: direct call to ZZDSKBSR.' )


      CALL ZZDSKBSR ( ' ',    1,      1,      ZZDSKBDC, DSKCTR, 
     .                UPDATE, DLADSC, DSKDSC, FOUND             )

      CALL CHCKXC ( .TRUE., 'SPICE(DSKBOGUSENTRY)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create two DSK files, each containing ' 
     .//           'data for bodies 499 and 599.'                 )


      DSKS(1) = 'zzdskbsr_test_1.bds'
      DSKS(2) = 'zzdskbsr_test_2.bds'

      DO I = 1, 2

         IF ( EXISTS(DSKS(I)) ) THEN

            CALL DELFIL ( DSKS(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         BODYID  = 499
         FRAME   = 'IAU_MARS'
         SURFID  = I
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )
    
C
C        Append segment.
C
         BODYID  = 599
         FRAME   = 'IAU_JUPITER'
         SURFID  = I

         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

       

C***********************************************************************
C
C     ZZDSKLSF/ZZDSKUSF/ZZDSKBSS/ZZDSKSNS basic tests
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSNS Error case: search for segments for '
     .//           'body 499 without first calling ZZDSKBSS.'      )

      BODYID = 499

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(CALLZZDSKBSSFIRST)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKBSS Error case: search for segments for '
     .//           'body 499. No DSKs have been loaded yet.'      )

      BODYID = 499

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: Load first DSK; search for segments for '
     .//           'body 499.'                                        )


C
C     Initialize the local DSK state counter.
C
      CALL ZZCTRUIN ( DSKCTR )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Load the first DSK file.
C
      CALL ZZDSKLSF ( DSKS(1), HANDLS(1) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that an update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      BODYID = 499

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     We should find one segment.
C
      CALL CHCKSL ( 'FOUND (1)', FOUND, .TRUE., OK )
      
      IF ( FOUND ) THEN
C
C        Check the other outputs.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(1), 0, OK )

         CALL DLABFS ( HANDLS(1), XDLADS, FND )
         CALL CHCKXC   ( .FALSE., ' ', OK )
         
         CALL CHCKSL ( 'DLABFS FOUND', FND, .TRUE., OK )

         CALL DSKGD ( HANDLS(1), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

         CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )

      END IF

C
C     Continue the search. We should not find another segment.
C
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (2)', FOUND, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: Search for segments for body 599.' )

      BODYID = 599


      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     We should find one segment.
C
      CALL CHCKSL ( 'FOUND (1)', FOUND, .TRUE., OK )
      
      IF ( FOUND ) THEN
C
C        Check the other outputs. We need to locate the second
C        segment in the file first.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(1), 0, OK )

         CALL DLABFS ( HANDLS(1), XDLADS, FND )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL DLAFNS ( HANDLS(1), XDLADS, NXTDSC, FND )
         CALL CHCKXC   ( .FALSE., ' ', OK )
         
         CALL MOVEI ( NXTDSC, DLADSZ, XDLADS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'DLAFNS FOUND', FND, .TRUE., OK )

         CALL DSKGD ( HANDLS(1), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

         CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )

      END IF

C
C     Continue the search. We should not find another segment.
C
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (2)', FOUND, .FALSE., OK )

C
C     Check the state change status. We expect that no update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Search for segments for body 699. None should '
     .//           'be found. No error should be signaled.'        )


      BODYID = 699


      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     We should find...nothing.
C
      CALL CHCKSL ( 'FOUND (1)', FOUND, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: Load second DSK; search for segments for '
     .//           'body 499.'                                         )

      CALL ZZDSKLSF ( DSKS(2), HANDLS(2) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that an update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      BODYID = 499

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      NSEG = 2
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
C
C        We should find two segments. The first one found should
C        be in the file loaded last.
C
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
         I = I    + 1
         J = NSEG + 1 - I

         IF ( FOUND ) THEN
C
C           Check the other outputs.
C               
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(J), 0, OK )
C
C           Search backwards in the file for the expected segment.
C           In each case we're looking for the first segment in
C           the file.
C
            CALL DLABBS ( HANDLS(J), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL DLAFPS ( HANDLS(J), XDLADS, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'DLAFPS FOUND', FND, .TRUE., OK )


            CALL DSKGD ( HANDLS(J), XDLADS, XDSKDS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( 'DLADSC', DLADSC, '=', 
     .                              XDLADS, DLADSZ, OK )

            CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', 
     .                              XDSKDS, DSKDSZ, 0.D0, OK )
         END IF

C
C        Fetch information for the next segment.
C
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: Keep both files loaded; search for '
     .//           'segments for body 599.'                       )

      BODYID = 599

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      NSEG = 2
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
C
C        We should find two segments. The first one found should
C        be in the file loaded last.
C
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
         I = I    + 1
         J = NSEG + 1 - I

         IF ( FOUND ) THEN
C
C           Check the other outputs.
C               
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(J), 0, OK )
C
C           Search backwards in the file for the expected segment.
C           In each case we're looking for the second segment in
C           the file.
C
            CALL DLABBS ( HANDLS(J), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'DLABBS FOUND', FND, .TRUE., OK )


            CALL DSKGD ( HANDLS(J), XDLADS, XDSKDS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( 'DLADSC', DLADSC, '=', 
     .                              XDLADS, DLADSZ, OK )

            CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', 
     .                              XDSKDS, DSKDSZ, 0.D0, OK )
         END IF

C
C        Fetch information for the next segment.
C
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: Load first DSK a second time; '
     .//           'verify that the handle is unchanged.'    )

      CALL ZZDSKLSF ( DSKS(1), HANDLE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(1), 0, OK ) 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: Load second DSK a second time; '
     .//           'verify that the handle is unchanged.'    )

      CALL ZZDSKLSF ( DSKS(2), HANDLE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(2), 0, OK ) 


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSF: Unload the first file; keep the '
     .//           'second file loaded; search for '
     .//           'segments for body 499.'                  )


      CALL ZZDSKUSF ( HANDLS(1) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that an update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      BODYID = 499

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     We should find one segment.
C
      CALL CHCKSL ( 'FOUND (1)', FOUND, .TRUE., OK )
      
      IF ( FOUND ) THEN
C
C        Check the other outputs.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(2), 0, OK )

         CALL DLABFS ( HANDLS(2), XDLADS, FND )
         CALL CHCKXC   ( .FALSE., ' ', OK )
         
         CALL CHCKSL ( 'DLABFS FOUND', FND, .TRUE., OK )

         CALL DSKGD ( HANDLS(2), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

         CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )

      END IF

C
C     Continue the search. We should not find another segment.
C
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (2)', FOUND, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSF: Continue the previous case; '
     .//           'search for segments for body 599.'     )

      BODYID = 599

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     We should find one segment.
C
      CALL CHCKSL ( 'FOUND (1)', FOUND, .TRUE., OK )
      
      IF ( FOUND ) THEN
C
C        Check the other outputs.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(2), 0, OK )

C
C        Use a backward search here.
C
         CALL DLABBS ( HANDLS(2), XDLADS, FND )
         CALL CHCKXC   ( .FALSE., ' ', OK )
         
         CALL CHCKSL ( 'DLABFS FOUND', FND, .TRUE., OK )

         CALL DSKGD ( HANDLS(2), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

         CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )

      END IF

C
C     Continue the search. We should not find another segment.
C
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND (2)', FOUND, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSF: unload the second file. Call '
     .//           'ZZDSKBSS to try to start a search for '
     .//           'segments for body 499.'                 )


      CALL ZZDSKUSF ( HANDLS(2) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that an update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )



      BODYID = 499

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: Load first DSK a third time; '
     .//           'verify that the handle is new.'    )

      CALL ZZDSKLSF ( DSKS(1), HANDLE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that an update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )



      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(2)+1, 0, OK ) 

      HANDLS(1) = HANDLE

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: Load second DSK a third time; '
     .//           'verify that the handle is unchanged.'    )

      CALL ZZDSKLSF ( DSKS(2), HANDLE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that an update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(1)+1, 0, OK ) 

      HANDLS(2) = HANDLE




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: After reload: search for segments for '
     .//           'body 499.'                                         )


C
C     We must first build a segment list for body 499, so the segments
C     will be found in the expected order.
C
      BODYID = 499

      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )


      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      NSEG = 2
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
C
C        We should find two segments. The first one found should
C        be in the file loaded last.
C
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
         I = I    + 1
         J = NSEG + 1 - I

         IF ( FOUND ) THEN
C
C           Check the other outputs.
C               
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(J), 0, OK )
C
C           Search backwards in the file for the expected segment.
C           In each case we're looking for the first segment in
C           the file.
C
            CALL DLABBS ( HANDLS(J), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL DLAFPS ( HANDLS(J), XDLADS, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'DLAFPS FOUND', FND, .TRUE., OK )


            CALL DSKGD ( HANDLS(J), XDLADS, XDSKDS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( 'DLADSC', DLADSC, '=', 
     .                              XDLADS, DLADSZ, OK )

            CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', 
     .                              XDSKDS, DSKDSZ, 0.D0, OK )

         END IF

C
C        Fetch information for the next segment.
C
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

       

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: After reload: search for '
     .//           'segments for body 599.'              )

      BODYID = 599
C
C     Rebuild the segment list for 599.
C     
      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Start a search.
C
      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      NSEG = 2
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
C
C        We should find two segments. The first one found should
C        be in the file loaded last.
C
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
         I = I    + 1
         J = NSEG + 1 - I

         IF ( FOUND ) THEN
C
C           Check the other outputs.
C               
            CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(J), 0, OK )
C
C           Search backwards in the file for the expected segment.
C           In each case we're looking for the second segment in
C           the file.
C
            CALL DLABBS ( HANDLS(J), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'DLABBS FOUND', FND, .TRUE., OK )


            CALL DSKGD ( HANDLS(J), XDLADS, XDSKDS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( 'DLADSC', DLADSC, '=', 
     .                              XDLADS, DLADSZ, OK )

            CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', 
     .                              XDSKDS, DSKDSZ, 0.D0, OK )
         END IF

C
C        Fetch information for the next segment.
C
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO




C***********************************************************************
C
C
C     ZZDSKLSF/ZZDSKBSS/ZZDSKSNS full capability tests
C
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create DSK containing data for BTSIZE+1'
     .//           'bodies. There are two segments per body.' )


      BTDSK = 'zzdskbsr_body_table.bds'

      IF ( EXISTS(BTDSK) ) THEN
         CALL DELFIL ( BTDSK )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

C
C     We want to take advantage of the SPICE built-in non-inertial
C     frames. We need BTSIZE+1 of them, so we must include the nonsense
C     frames such as those associated with planetary barycenters.
C     This doesn't matter from the perspective of this test.
C
C
      NBOD = BTSIZE + 1

      N    = 0
      I    = 1


      DO WHILE ( ( N .LT. NBOD ) .AND. ( I .LT. BTSIZE+10 ) )

         FRAMID = 10000 + I

         CALL FRINFO ( FRAMID, BODYID, CLASS, CLSSID, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Note that there's a gap in the sequence for class ID 3000;
C        there may be other gaps.
C
C        Also, we want to avoid using the earth, since IAU_EARTH
C        and EARTH_FIXED are both built-in frames centered on
C        the earth. This is an unnecessary complication.
C        
         IF (  FOUND .AND. ( BODYID .NE. 399 )  ) THEN

            N = N + 1

            CALL FRMNAM ( FRAMID, FRAME )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            IF ( FRAME .EQ. ' ' ) THEN
               CALL SETMSG ( 'Frame name for ID # is blank.' )
               CALL ERRINT ( '#', FRAMID                     )
               CALL SIGERR ( 'SPICE(NOTRANSLATION)'          )
               CALL CHCKXC ( .FALSE., ' ', OK )
            END IF

            SURFID = N

            CALL T_SMLDSK ( BODYID, SURFID, FRAME, BTDSK )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            SURFID = -N

            CALL T_SMLDSK ( BODYID, SURFID, FRAME, BTDSK )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CENTRS(N) = BODYID

         END IF
         
         I = I + 1

      END DO

      CALL CHCKSI ( 'N', N, '=', NBOD, 0, OK )

       

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Load the DSK that contains BTSIZE+1 bodies. '
     .//           'Look up data for each one.'                )


C
C     This test will force one body out of the body list.
C      
      CALL ZZDSKLSF ( BTDSK, BTHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = NBOD, 1, -1

         BODYID = CENTRS(I)

         CALL ZZDSKBSS ( BODYID )
         CALL CHCKXC   ( .FALSE., ' ', OK )

C
C        Prepare segment selection test function for use in BSR search.
C      
         RETVAL = ZZDSKSBD ( BODYID )
         CALL CHCKXC   ( .FALSE., ' ', OK )

C
C        Perform the search. We expect to find two segments per body.
C
         DO J = 1, 2

            CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'ZZDSKSNS FOUND', FOUND, .TRUE., OK )

            IF ( FOUND ) THEN
C
C              Check the other outputs.
C               
               LABEL = 'HANDLE I=@; J=@'
               CALL REPMI ( LABEL, '@', I, LABEL )
               CALL REPMI ( LABEL, '@', J, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKSI ( LABEL, HANDLE, '=', BTHAN, 0, OK )

C
C              Search backwards in the file for the expected segment.
C
               IF ( ( I .EQ. NBOD ) .AND. ( J .EQ. 1 ) ) THEN

                  CALL DLABBS ( BTHAN, XDLADS, FND )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'DLABBS FOUND', FND, .TRUE., OK )

               ELSE

                  CALL DLAFPS ( BTHAN, XDLADS, PRVDSC, FND )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  CALL CHCKSL ( 'DLAFPS FOUND', FND, .TRUE., OK )
   
                  CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

               END IF

               CALL DSKGD ( BTHAN, XDLADS, XDSKDS )
               CALL CHCKXC   ( .FALSE., ' ', OK )


               LABEL = 'DLADSC I=@; J=@'
               CALL REPMI ( LABEL, '@', I, LABEL )
               CALL REPMI ( LABEL, '@', J, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKAI ( LABEL, DLADSC, '=', 
     .                              XDLADS, DLADSZ, OK )

               LABEL = 'DSKDSC I=@; J=@'
               CALL REPMI ( LABEL, '@', I, LABEL )
               CALL REPMI ( LABEL, '@', J, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKAD ( LABEL, DSKDSC, '=', 
     .                              XDSKDS, DSKDSZ, 0.D0, OK )

            END IF

         END DO

      END DO


C
C     Unload this file to simplify the later tests.
C
      CALL ZZDSKUSF ( BTHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create FTSIZE+1 DSKs containing data '
     .//           'for the earth. Each file but the last '
     .//           'contains two segments. The last one contains '
     .//           'five segments.'                               )


      NEARTH = FTSIZE + 1

      DO I = 3, 2 + NEARTH

         DSKS(I) =  'zzdskbsr_test_#.bds'
         CALL REPMI ( DSKS(I), '#', I, DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( EXISTS(DSKS(I)) ) THEN

            CALL DELFIL ( DSKS(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END IF

         BODYID  = 399
         FRAME   = 'IAU_EARTH'
         SURFID  = I
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         SURFID  = -I
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO       

      I = 2 + NEARTH
      
      DO J = 1, 3

         SURFID = J
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: load FTSIZE-2 earth DSKs.' )

C
C     Note that DSKs 1 and 2 are still loaded.
C      
      DO I = 3, FTSIZE

         CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

C
C        Check the state change status. We expect that an update is
C        indicated.
C
         CALL ZZDSKCHK ( DSKCTR, UPDATE )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      END DO

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF error case: try to load one more '
     .//           'earth DSK.' )

      I = FTSIZE + 1

      CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
      CALL CHCKXC   ( .TRUE., 'SPICE(FTFULL)', OK )
C
C     The error message above brought to you by ZZDDHOPN.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKLSF: unload first two DSKs and load '
     .//           'remaining earth DSKs, except for the last one.' )


      DO I = 1, 2

         CALL ZZDSKUSF ( HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )
C
C        Check the state change status. We expect that an update is
C        indicated.
C
         CALL ZZDSKCHK ( DSKCTR, UPDATE )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      END DO

      DO I = 1, 2
         
         J = FTSIZE + I

         CALL ZZDSKLSF ( DSKS(J), HANDLS(J) )

         CALL CHCKXC   ( .FALSE., ' ', OK )
C
C        Check the state change status. We expect that an update is
C        indicated.
C
         CALL ZZDSKCHK ( DSKCTR, UPDATE )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      END DO
      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: prepare to look up all earth '
     .//           'segments but those in the last earth file. '
     .//           'Force ZZDSKBSR to build a segment list '
     .//           'for the earth via a call to ZZDSKBBL.'            )

      BODYID = 399

       

      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that no update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )

        
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: look up all loaded '
     .//           'earth segments ' )

C
C     Note that the last earth file is not loaded.
C

      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that no update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )


C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      NSEG = STSIZE
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
C
C        We should find STSIZE segments.
C
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
         I      = I    + 1
         J      = NSEG + 1 - I
         FILENO = 2 + ( (J - 1)/2  ) + 1
         
C
C        Check the state change status. We expect that no update is
C        indicated.
C
         CALL ZZDSKCHK ( DSKCTR, UPDATE )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )

C
C        Check the other outputs.
C               
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(FILENO), 0, OK )

         IF ( ODD(I) ) THEN
C
C           Start a backward search for the expected
C           segment.
C
            CALL DLABBS ( HANDLS(FILENO), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( '(1) DLABFS FOUND', FND, .TRUE., OK )

         ELSE
C
C           Continue the backward search for the
C           expected segment.
C
            CALL DLAFPS ( HANDLS(FILENO), DLADSC, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

         END IF

         CALL DSKGD ( HANDLS(FILENO), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( '(1) DLADSC', DLADSC, '=', 
     .                               XDLADS, DLADSZ, OK )

         CALL CHCKAD ( '(1) DSKDSC', DSKDSC, '=', 
     .                               XDSKDS, DSKDSZ, 0.D0, OK )

C
C        Fetch information for the next segment.
C
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO
      
C
C     We should have found STSIZE segments.
C
      CALL CHCKSI ( 'num. segments found', I, '=', STSIZE, 0, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: repeat look up.' )


      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that no update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )


C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      NSEG = STSIZE
      I    = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
C
C        We should find STSIZE segments.
C
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
         I      = I    + 1
         J      = NSEG + 1 - I
         FILENO = 2 + ( (J - 1)/2  ) + 1
         
C
C        Check the state change status. We expect that no update is
C        indicated.
C
         CALL ZZDSKCHK ( DSKCTR, UPDATE )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )

C
C        Check the other outputs.
C               
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(FILENO), 0, OK )

         IF ( ODD(I) ) THEN
C
C           Start a backward search for the expected
C           segment.
C
            CALL DLABBS ( HANDLS(FILENO), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( '(1) DLABFS FOUND', FND, .TRUE., OK )

         ELSE
C
C           Continue the backward search for the
C           expected segment.
C
            CALL DLAFPS ( HANDLS(FILENO), DLADSC, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

         END IF

         CALL DSKGD ( HANDLS(FILENO), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( '(1) DLADSC', DLADSC, '=', 
     .                               XDLADS, DLADSZ, OK )

         CALL CHCKAD ( '(1) DSKDSC', DSKDSC, '=', 
     .                               XDSKDS, DSKDSZ, 0.D0, OK )

C
C        Fetch information for the next segment.
C
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO
      
C
C     We should have found STSIZE segments.
C
      CALL CHCKSI ( 'num. segments found', I, '=', STSIZE, 0, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: unload even-indexed ' 
     .//           'earth DSKs. Look up remaining segments. ' )


      DO I = 4, 2+FTSIZE, 2

         CALL ZZDSKUSF ( HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )
         
      END DO

C
C     Check the state change status. We expect that an update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

C
C     Start a new search.
C     
      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )
C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      NSEG = STSIZE 

C
C     I is the segment offset from the head of the list. I is 1-based.
C     K is the iteration count.

      I = 0
      K = 0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
C
C        We should find STSIZE/2 segments.
C      
         K = K + 1

         IF ( EVEN(I) ) THEN

            I = I + 3
         ELSE
            I = I + 1
         END IF

         J      = NSEG + 1 - I
         FILENO = 2 + ( (J - 1)/2  ) + 1
         
C
C        Check the other outputs.
C               
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(FILENO), 0, OK )

         IF ( EVEN(J) ) THEN
C
C           Start a backward search in the file at index FILENO for the
C           expected segment.
C
            CALL DLABBS ( HANDLS(FILENO), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( '(1) DLABFS FOUND', FND, .TRUE., OK )

         ELSE
C
C           Continue the backward search for the
C           expected segment.
C
            CALL DLAFPS ( HANDLS(FILENO), DLADSC, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

         END IF

         CALL DSKGD ( HANDLS(FILENO), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( '(1) DLADSC', DLADSC, '=', 
     .                               XDLADS, DLADSZ, OK )

         CALL CHCKAD ( '(1) DSKDSC', DSKDSC, '=', 
     .                               XDSKDS, DSKDSZ, 0.D0, OK )

C
C        Fetch information for the next segment.
C
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO
      
C
C     We should have found STSIZE/2 segments.
C
      CALL CHCKSI ( 'num. segments found', K, '=', STSIZE/2, 0, OK )

       

C
C     Reload the earth kernels indexed 1 through FTSIZE to restore
C     the original file priorities.
C
      DO I = 3, FTSIZE+2

         CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

C
C     Force construction of a new segment list.
C
      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: unload odd-indexed ' 
     .//           'earth DSKs. Look up remaining segments. ' )


      DO I = 3, 2+FTSIZE, 2

         CALL ZZDSKUSF ( HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )
         
      END DO

C
C     Check the state change status. We expect that an update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

C
C     Start a new search.
C     
      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )
C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      NSEG = STSIZE 

C
C     I is the segment offset from the head of the list. I is 1-based.
C     K is the iteration count.

      I = -2
      K =  0

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO WHILE ( FOUND )
C
C        We should find STSIZE/2 segments.
C      
         K = K + 1


         IF ( EVEN(I) ) THEN

            I = I + 3
         ELSE
            I = I + 1
         END IF

         J      = NSEG + 1 - I
         FILENO = 2 + ( (J - 1)/2  ) + 1

         
C
C        Check the other outputs.
C               
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(FILENO), 0, OK )

         IF ( EVEN(J) ) THEN
C
C           Start a backward search in the file at index FILENO for the
C           expected segment.
C
            CALL DLABBS ( HANDLS(FILENO), XDLADS, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSL ( '(1) DLABFS FOUND', FND, .TRUE., OK )

         ELSE
C
C           Continue the backward search for the
C           expected segment.
C
            CALL DLAFPS ( HANDLS(FILENO), DLADSC, PRVDSC, FND )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
            CALL CHCKXC   ( .FALSE., ' ', OK )

         END IF

         CALL DSKGD ( HANDLS(FILENO), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( '(1) DLADSC', DLADSC, '=', 
     .                               XDLADS, DLADSZ, OK )

         CALL CHCKAD ( '(1) DSKDSC', DSKDSC, '=', 
     .                               XDSKDS, DSKDSZ, 0.D0, OK )

C
C        Fetch information for the next segment.
C
         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO
      
C
C     We should have found STSIZE/2 segments.
C
      CALL CHCKSI ( 'num. segments found', K, '=', STSIZE/2, 0, OK )

      
C
C     Reload the earth kernels indexed 1 through FTSIZE to restore
C     the original file priorities.
C
      DO I = 3, FTSIZE+2

         CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO


C
C     Force construction of a new segment list.
C
      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

     

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unload the first earth DSK. '
     .//           'Load the final earth DSK. Attempt a search.' )
C
C     The segment list should run out of room while the front of the
C     list is built. This should force the reconstruction of the list,
C     with eventual failure when the tail of the list cannot be 
C     accommodated.
C
      CALL ZZDSKUSF ( HANDLS(3) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      K = 2 + NEARTH

      CALL ZZDSKLSF ( DSKS(K), HANDLS(K) )
      CALL CHCKXC   ( .FALSE., ' ', OK )


C
C     Check the state change status. We expect that no update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search over all segments. We expect a failure.
C
C     Use ZZDSKNOT to force a complete search.
C
      
      CALL ZZDSKSNS ( ZZDSKNOT, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .TRUE., 'SPICE(BUFFEROVERFLOW)', OK )

        

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Search for one earth segment. It should be '
     .//           'located in the last file loaded.'            )



      K = 2 + NEARTH

      CALL ZZDSKLSF ( DSKS(K), HANDLS(K) )
      CALL CHCKXC   ( .FALSE., ' ', OK )
C
C     Check the state change status. We expect that an update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      BODYID = 399

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that no update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE., OK )


C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      NSEG = STSIZE

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Fetch data to compare.
C
      CALL DLABBS ( HANDLS(K), XDLADS, FND )
      CALL CHCKXC   ( .FALSE., ' ', OK )
         
      CALL CHCKSL ( 'DLABFS FOUND', FND, .TRUE., OK )

C     
C     Check the other outputs.
C
      CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(K), 0, OK )


      CALL DSKGD ( HANDLS(K), XDLADS, XDSKDS )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

      CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Look up earth segments until we encounter a '
     .//           'failure. We don''t have enough room in the '
     .//           'segment table for all earth segments.'       )

  
      BODYID = 399
 
      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )
 
      CALL ZZDSKSNS ( ZZDSKNOT, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .TRUE., 'SPICE(BUFFEROVERFLOW)', OK )

C
C     Unload the last earth DSK.
C
      CALL ZZDSKUSF ( HANDLS(2+NEARTH) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Restore the first FTSIZE earth DSKs.
C
      DO I = 3, FTSIZE+2

         CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

C
C     Force construction of a new segment list.
C
      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup: create a DSK file containing 10 '
     .//           'segments for body 699.'                    )


C
C     To even create a new DSK, we need to unload one of them,
C     since we'll open a new DSK. We normally would need to 
C     unload two files to accommodate the scratch DAS used for
C     DAS segregation, but T_SMLDSK doesn't segregate its output
C     file.
C
      CALL ZZDSKUSF ( HANDLS(FTSIZE+2) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that an update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      NSAT = 10


      I = FTSIZE + 3

      DSKS(I) = 'zzdskbsr_test_#.bds'
      CALL REPMI ( DSKS(I), '#', I, DSKS(I) )

      IF ( EXISTS(DSKS(I)) ) THEN

         CALL DELFIL ( DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      BODYID  = 699
      FRAME   = 'IAU_SATURN'

      DO J = 1, NSAT

         SURFID  = J
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, DSKS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKBSS/ZZDSKSNS: load '
     .//           'the Saturn DSK. Look up the Saturn segments. '
     .//           'This look-up should cause the 399, 499, '
     .//           'and 599 segment lists to be dumped.'          )

C
C     Load the first DSK and find data for 499 and 599, so we have 
C     segment lists that need to be removed. We have too many files
C     loaded, so unload an earth file first.
C
      CALL ZZDSKUSF ( HANDLS(3) )
      CALL CHCKXC   ( .FALSE., ' ', OK )
      
      CALL ZZDSKLSF ( DSKS(1), HANDLS(1) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKBBL ( 499 )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKBBL ( 599 )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      I = FTSIZE + 3

      CALL ZZDSKLSF ( DSKS(I), HANDLS(I) )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Check the state change status. We expect that an update is
C     indicated.
C
      CALL ZZDSKCHK ( DSKCTR, UPDATE )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )


      BODYID = 699

      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Prepare segment selection test function for use in BSR search.
C      
      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     Perform the search.
C
      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     We should find NSAT segments from DSK #(FTSIZE+3).
C
      CALL CHCKSL ( 'FOUND (1)', FOUND, .TRUE., OK )
      CALL DLABBS ( HANDLS(I), XDLADS, FND )
      CALL CHCKXC   ( .FALSE., ' ', OK )
         
      CALL CHCKSL ( 'DLABFS FOUND', FND, .TRUE., OK )
     
      J = 0

      DO WHILE ( FOUND .AND. FND .AND. ( J .LT. NSAT) )

         J = J + 1
C
C        Check the other outputs.
C
         CALL CHCKSI ( 'HANDLE', HANDLE, '=', HANDLS(I), 0, OK )


         CALL DSKGD ( HANDLS(I), XDLADS, XDSKDS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL CHCKAI ( 'DLADSC', DLADSC, '=', XDLADS, DLADSZ, OK )

         CALL CHCKAD ( 'DSKDSC', DSKDSC, '=', XDSKDS, DSKDSZ, 0.D0, OK )


C
C        Continue to search backwards.
C
         CALL DLAFPS ( HANDLS(I), XDLADS, PRVDSC, FND ) 
         CALL CHCKXC   ( .FALSE., ' ', OK )
         
         CALL MOVEI ( PRVDSC, DLADSZ, XDLADS )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
         CALL CHCKXC   ( .FALSE., ' ', OK )
         
      END DO
 

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKUSF test: cause 699 body table entry '
     .//           'to be deleted, when 699 is not at index NBT' )

C
C     To set this up, we'll put the earth body table entry at
C     the end of the table. We need to do an earth lookup to 
C     get the earth back into the body table, while avoiding
C     removal of the Saturn list. Make the set of earth segments
C     smaller by unloading some earth files.
C
      DO I = 3, 12
         CALL ZZDSKUSF ( HANDLS(I) )
         CALL CHCKXC   ( .FALSE., ' ', OK )
      END DO
C
C     Now do an earth search.
C     
      BODYID = 399
      CALL ZZDSKBSS ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      RETVAL = ZZDSKSBD ( BODYID )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKSNS ( ZZDSKBDC, HANDLE, DLADSC, DSKDSC, FOUND ) 
      CALL CHCKXC   ( .FALSE., ' ', OK )

C
C     All of the preceding was to get the earth into the last 
C     position of the body table. Now unload the Saturn file.
C
      K = FTSIZE + 3

      CALL ZZDSKUSF ( HANDLS(K) )
      CALL CHCKXC   ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDSKSNS test: search a file that has more ' 
     .//           'than STSIZE segments.'                      )

C
C     Create a monster DSK.
C
      OVRDSK = 'zzdskbsr_toobig.bds'


      IF ( EXISTS(OVRDSK) ) THEN

         CALL DELFIL ( OVRDSK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      BODYID  = 999

      DO I = 1, STSIZE + 1

         FRAME   = 'IAU_PLUTO'
         SURFID  = I
         CALL T_SMLDSK ( BODYID, SURFID, FRAME, OVRDSK )
         CALL CHCKXC   ( .FALSE., ' ', OK )

      END DO

C
C     Try to build a segment list for 999.
C     
      CALL ZZDSKLSF ( OVRDSK, OVRHAN )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKBBL ( BODYID )
      CALL CHCKXC   ( .TRUE., 'SPICE(BUFFEROVERFLOW)', OK )


C
C     Clean up.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up.' )

   

      DO I = 1, FTSIZE + 3

         CALL ZZDSKUSF ( HANDLS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DELFIL ( DSKS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZDSKUSF ( BTHAN )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL DELFIL   ( BTDSK )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL ZZDSKUSF ( OVRHAN )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL DELFIL   ( OVRDSK )
      CALL CHCKXC   ( .FALSE., ' ', OK )

 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


