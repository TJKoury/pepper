C$Procedure F_KPBUG ( UNLOAD bug demonstration )
 
      SUBROUTINE F_KPBUG ( OK )
 
C$ Abstract
C
C     Demonstrate the HANDLE bug in keeper
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
C     This routine tests the handle-changing bug in the following 
C     call sequence to FURNSH and UNLOAD
C     type 20 SPK data:
C
C     CALL FURNSH(SPK0)
C     CALL KINFO(SPK0, KTYPE, SSRC, HAN0, FOUND0)
C     CALL FURNSH(SPK0)
C     CALL KINFO(SPK0, KTYPE, SSRC, HAN1, FOUND1)
C     CALL UNLOAD(SPK0)
C     CALL KINFO(SPK0, KTYPE, SSRC, HAN2, FOUND2)
C
C   Now HAN2 != HAN0 and HAN1 but FOUND<n> are
C   all TRUE.  
C

C
C        
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
C     G.M. Hockney     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 27-JAN-2017 (NJB) (GMH)
C
C        This routine has been updated so that, prior to exit,
C        it deletes the SPK it creates.
C        
C        Deleted declarations of unused functions.
C
C        Renamed George's routine from FF_KEEPBUG 
C        to F_KPBUG.
C
C     Original Version 1.0.0, 19-MAY-2015 (GMH)
C
C
C-&
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C     
      LOGICAL               EXISTS

C
C     Local Parameters
C
      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0   = 'test.bsp' )

      INTEGER              FILSIZ
      PARAMETER          ( FILSIZ = 255 )

      INTEGER              TYPSIZ
      PARAMETER          ( TYPSIZ = 4 )

C
C     Local Variables
C      
      CHARACTER*(FILSIZ)    KTYPE
      CHARACTER*(TYPSIZ)    KSRC

      INTEGER               HAN0
      INTEGER               HAN1
      INTEGER               HAN2
      INTEGER               HAN3
 
      LOGICAL               FOUND0
      LOGICAL               FOUND1
      LOGICAL               FOUND2
      LOGICAL               FOUND3


C
C     Saved variables
C     
C
C     Save all local variables to avoid stack problems on some
C     platforms.
C
      SAVE


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_KPBUG' )      


C
C     Open a new SPK file for writing.
C     
C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Setup: create kernels.' )


      CALL TSTLSK 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create SPK file.' )
C
C     Create a small SPK file containing 1 record. We'll
C     use the moon as the target and the earth-moon barycenter
C     as the center.
C

C
C     Load test SPK to provide data.
C
      IF ( EXISTS(SPK0) ) THEN
         CALL DELFIL( SPK0 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTSPK ( SPK0, .TRUE., HAN0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HAN0 )
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Demonstrate multiple load bug.' )

      CALL FURNSH(SPK0)
      CALL KINFO(SPK0, KTYPE, KSRC, HAN0, FOUND0)
      CALL CHCKSL ( 'KEEPBUG 0', FOUND0, .TRUE., OK )

      CALL FURNSH(SPK0)
      CALL KINFO(SPK0, KTYPE, KSRC, HAN1, FOUND1)
      CALL CHCKSL ( 'KEEPBUG 1', FOUND1, .TRUE., OK )
      CALL CHCKSI ( 'KEEPBUG 2', HAN0, '=', HAN1, 0, OK )

      CALL UNLOAD(SPK0)
      CALL KINFO(SPK0, KTYPE, KSRC, HAN2, FOUND2)
      CALL CHCKSL ( 'KEEPBUG 3', FOUND2, .TRUE., OK )
      CALL CHCKSI ( 'KEEPBUG 4', HAN0, '=', HAN2, 0, OK )

      CALL UNLOAD(SPK0)
      CALL KINFO(SPK0, KTYPE, KSRC, HAN3, FOUND3)
      CALL CHCKSL ( 'KEEPBUG 5', FOUND3, .FALSE., OK )


      IF ( EXISTS(SPK0) ) THEN
         CALL DELFIL( SPK0 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_SUCCESS ( OK )
      RETURN
      END
