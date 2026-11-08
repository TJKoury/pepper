C$Procedure F_DDHISN ( ZZDDHISN Test Family )
 
      SUBROUTINE F_DDHISN ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHISN
C     routine.
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
 
      INCLUDE              'zzddhman.inc'
 
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
C$ Files
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
C$ Particulars
C
C     This routine exercises ZZDDHISN's logic.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
C
C        Updated module as the result of changes to the handle
C        manager interface, ZZDDHCLS.
C
C-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               EQSTR
 
C
C     Local Variables
C
      CHARACTER*(20)        ARCH
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMPAT
      CHARACTER*(20)        METHOD
      CHARACTER*(STRSIZ)    STRNAT
      CHARACTER*(STRSIZ)    STRNOW
      CHARACTER*(STRLEN)    RDSBFF
 
      INTEGER               HANDLE ( NUMBFF )
      INTEGER               I
      INTEGER               NATFID
      INTEGER               NUMFIL
 
      LOGICAL               FOUND
      LOGICAL               NATIVE
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHISN' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'F_DDHISN Initialization' )
 
C
C     Get the native binary file format for this platform.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', STRNAT )
      CALL UCASE ( STRNAT, STRNAT )
 
C
C     Get the list of supported binary file formats for this platform.
C
      CALL ZZPLATFM ( 'READS_BFF', RDSBFF )
      CALL UCASE ( RDSBFF, RDSBFF )
 
C
C     Set the filename pattern we will use.
C
      FNMPAT = 'test#.fil'
 
C
C     Now create a file of each supported type.  We are just going to
C     trick the system into thinking it's an actual file of this type.
C
      I = 1
      CALL NEXTWD ( RDSBFF, STRNOW, RDSBFF )
 
      DO WHILE ( STRNOW .NE. ' ' )
 
C
C        Build the name of the file we intend to use.
C
         CALL REPMI ( FNMPAT, '#', I, FNAME )
 
C
C        Check to see if this is the native format.
C
         IF ( EQSTR( STRNOW, STRNAT ) ) THEN
            NATFID = I
         END IF
 
C
C        Create the file.
C
         CALL T_CPTFIL ( FNAME,
     .                   DAF,
     .                   2,
     .                   STRNOW,
     .                   'ABCD',
     .                   'EFGH',
     .                   'IJKL',
     .                   .TRUE.,
     .                   .FALSE.,
     .                   'DAF/SPK '     )
 
C
C        Open the file into the handle manager.
C
         METHOD = 'READ'
         ARCH   = 'DAF'
 
         CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE(I) )
 
C
C        Strip off the next word from RDSBFF and increment I.
C
         CALL NEXTWD ( RDSBFF, STRNOW, RDSBFF )
         I = I + 1
 
      END DO
 
C
C     Store the number of files we placed into the handle list.
C
      NUMFIL = I - 1
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now that we have finished all of the initialization stuff,
C     break the tests into two cases.  NATIVE files and NON-NATIVE
C     files.
C
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exercise Native File Logic' )
 
C
C     Setup the inputs and outputs.
C
      NATIVE = .FALSE.
      FOUND  = .FALSE.
 
C
C     Invoke the module.
C
      CALL ZZDDHISN ( HANDLE(NATFID), NATIVE, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'NATIVE', NATIVE, .TRUE., OK )
      CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE., OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exercise Non-Native File Logic' )
 
      DO I = 1, NUMFIL
 
C
C        First check to see that this file is not of the native
C        format.
C
         IF ( I .NE. NATFID ) THEN
 
C
C           Setup the inputs and outputs.
C
            NATIVE = .TRUE.
            FOUND  = .FALSE.
 
C
C           Invoke the module.
C
            CALL ZZDDHISN ( HANDLE(I), NATIVE, FOUND )
 
C
C           Check for the absence of an exception.
C
            CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C           Check outputs.
C
            CALL CHCKSL ( 'NATIVE', NATIVE, .FALSE., OK )
            CALL CHCKSL ( 'FOUND',  FOUND,  .TRUE.,  OK )
 
         END IF
 
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exercise Unknown Handle Exception Logic' )
 
C
C     Setup inputs and outputs for the zero-valued handle test.
C
      NATIVE = .FALSE.
      FOUND  = .TRUE.
 
C
C     Invoke the module.
C
      CALL ZZDDHISN ( 0, NATIVE, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the outputs.  Since FOUND is FALSE, NATIVE should remain
C     unchanged.
C
      CALL CHCKSL ( 'NATIVE', NATIVE, .FALSE., OK )
      CALL CHCKSL ( 'FOUND',  FOUND,  .FALSE., OK )
 
C
C     Setup the inputs and outputs for the opposite-sign handle test.
C
      NATIVE = .TRUE.
      FOUND  = .FALSE.
 
C
C     Invoke the module.
C
      CALL ZZDDHISN ( -HANDLE(1), NATIVE, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'NATIVE', NATIVE, .TRUE.,  OK )
      CALL CHCKSL ( 'FOUND',  FOUND,  .FALSE., OK )
 
C
C ----------------------------------------------------------------
C
C     Now clean up.
C
      DO I = 1, NUMFIL
         CALL ZZDDHCLS ( HANDLE(I), ARCH, .FALSE. )
         CALL REPMI    ( FNMPAT, '#', I, FNAME )
         CALL KILFIL   ( FNAME )
      END DO
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
