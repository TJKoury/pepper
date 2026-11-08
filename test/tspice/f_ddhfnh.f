C$Procedure F_DDHFNH ( ZZDDHFNH Test Family )
 
      SUBROUTINE F_DDHFNH ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHFNH
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
C     This routine exercises ZZDDHFNH's logic.
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
C-    TSPICE Version 1.0.0, 06-NOV-2001 (FST)
C
C-&
 
C
C     Local Variables
C
      CHARACTER*(FILEN)     ALTFNM
      CHARACTER*(20)        BINFMT
      CHARACTER*(20)        ARCH
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(20)        METHOD
 
      INTEGER               ALTHAN
      INTEGER               HANDLE
 
      LOGICAL               FOUND
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHFNH' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'F_DDHFNH Initialization' )
 
C
C     Setup the test cases.
C
      FNAME  = 'test.fil'
      ARCH   = 'DAF'
      METHOD = 'READ'
      HANDLE = 0
      FOUND  = .TRUE.
 
C
C     Get the native binary file format string to utilize in
C     creating the test file.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', BINFMT )
 
      CALL KILFIL ( FNAME )
 
C
C     Create FNAME.
C
      CALL T_CPTFIL ( FNAME,
     .                DAF,
     .                2,
     .                BINFMT,
     .                'ABCD',
     .                'EFGH',
     .                'IJKL',
     .                .TRUE.,
     .                .FALSE.,
     .                'DAF/CK  '  )
 
C
C     Open the file.
C
      CALL ZZDDHOPN ( FNAME, METHOD, ARCH, HANDLE )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exercise Nominal ZZDDHFNH Logic' )
 
C
C     Now exercise ZZDDHFNH.
C
      CALL ZZDDHFNH ( FNAME, ALTHAN, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,  OK            )
      CALL CHCKSI ( 'ALTHAN', ALTHAN, '=',    HANDLE, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exercise LJUST of FNAME Logic' )
 
C
C     Now exercise the insignificance of leading white space.
C
      ALTFNM = '       ' // FNAME
 
      CALL ZZDDHFNH ( ALTFNM, ALTHAN, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,  OK            )
      CALL CHCKSI ( 'ALTHAN', ALTHAN, '=',    HANDLE, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exercise Blank Filename Exception' )
 
C
C     Lastly exercise 2 "NOT FOUND" cases.
C
      CALL ZZDDHFNH ( '     ', ALTHAN, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK       )
      CALL CHCKSI ( 'ALTHAN', ALTHAN, '=',    0, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exercise Unknown Filename Exception' )
 
      CALL ZZDDHFNH ( 'BOGUS_FILENAME_GOES_HERE', ALTHAN, FOUND )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check outputs.
C
      CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK       )
      CALL CHCKSI ( 'ALTHAN', ALTHAN, '=',    0, 0, OK )
 
C
C     Clean up.
C
      CALL ZZDDHCLS ( HANDLE, ARCH, .FALSE. )
      CALL KILFIL   ( FNAME        )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
 
      END
