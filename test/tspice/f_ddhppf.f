C$Procedure F_DDHPPF ( ZZDDHPPF Test Family )
 
      SUBROUTINE F_DDHPPF ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDDHPPF
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
C     This routine exercises ZZDDHPPF's logic.  There are a few IOSTAT
C     driven exceptions that are not readily exercised with the
C     delivered source code.  Also, there is a SPICE(BUG) exception
C     that requires the substitution of ZZPLATFM.  This testcode
C     does not address their execution.
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
C-    TSPICE Version 2.0.0, 06-FEB-2002 (FST)
C
C        This test family was altered to exercise updated logic in
C        ZZDDHPPF.  See the Revisions section of its header for a
C        detailed discussion of the changes.
C
C-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               ISRCHC
 
C
C     Local Variables
C
      CHARACTER*(4)         CFDREC
      CHARACTER*(4)         CNI
      CHARACTER*(8)         CNSUM
      CHARACTER*(STRSIZ)    NATIVE
      CHARACTER*(8)         NULBFF
      CHARACTER*(1)         NULL
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
 
      INTEGER               ARC
      INTEGER               BFF
      INTEGER               FDREC
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               NATBFF
      INTEGER               UNIT
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DDHPPF' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'F_DDHPPF Initialization' )
 
C
C     Now retrieve the list of supported binary file formats.
C
      DO I = 1, NUMBFF
         CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
      END DO
 
C
C     Setup the null byte character.
C
      NULL = CHAR(0)
 
C
C     Initialize NULBFF.
C
      DO I = 1, 8
         NULBFF(I:I) = NULL
      END DO
 
C
C     Check for the absence of a rogue exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid architecture code exception.' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = -1
      BFF = 1
 
C
C     Invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFILARC)', OK )
 
C
C     Check BFF.
C
      CALL CHCKSI ( 'BFF', BFF, '=', 0, 0, OK )
 
C
C     Setup the inputs and outputs for the other bound.
C
      ARC = NUMARC + 1
      BFF = 1
 
C
C     Invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFILARC)', OK )
 
C
C     Check BFF.
C
      CALL CHCKSI ( 'BFF', BFF, '=', 0, 0, OK )
 
C
C
C     We have commented this case out because it leaves a "fort.#"
C     file around on some systems.
C
C --- Case: ------------------------------------------------------
C
C     CALL TCASE ( 'File read failure.' )
C
C
C     Setup the inputs and outputs.
C
C     CALL GETLUN ( UNIT )
C
C     ARC = DAF
C     BFF = BIGI3E
C
C
C     Invoke the module.
C
C     CALL ZZDDHPPF ( UNIT, ARC, BFF )
C
C
C     Check for the exception.
C
C     CALL CHCKXC ( .TRUE., 'SPICE(FILEREADFAILED)', OK )
C
C
C     Check BFF.
C
C     CALL CHCKSI ( 'BFF', BFF, '=', 0, 0, OK )
C
C
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Unknown File Architecture.' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = BIGI3E
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, 2, 'BIG-IEEE', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .FALSE., 'TEST/FIL'  )
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFILARC)', OK )
 
C
C     Now check BFF.
C
      CALL CHCKSI ( 'BFF', BFF, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAS File Architecture, should be DAF.' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = BIGI3E
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, 2, 'BIG-IEEE', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .FALSE., 'DAS/EK  '  )
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCHMISMATCH)', OK )
 
C
C     Now check BFF.
C
      CALL CHCKSI ( 'BFF', BFF, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DAF File Architecture, should be DAS.' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAS
      BFF = BIGI3E
 
      CALL KILFIL ( 'testdas.das' )
 
      CALL T_CPTFIL ( 'testdas.das', ARC, 2, 'BIG-IEEE', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .FALSE., 'DAF/SPK '  )
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdas.das',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FILARCHMISMATCH)', OK )
 
C
C     Now check BFF.
C
      CALL CHCKSI ( 'BFF', BFF, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdas.das' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. Error in FTP check, DAF.' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = BIGI3E
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, 2, 'BIG-IEEE', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .TRUE., 'DAF/SPK '  )
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FTPXFERERROR)', OK )
 
C
C     Now check BFF.
C
      CALL CHCKSI ( 'BFF', BFF, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. Error in FTP check, DAS.' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAS
      BFF = BIGI3E
 
      CALL KILFIL ( 'testdas.das' )
 
      CALL T_CPTFIL ( 'testdas.das', ARC, 2, 'BIG-IEEE', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .TRUE., 'DAS/EK  '  )
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdas.das',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(FTPXFERERROR)', OK )
 
C
C     Now check BFF.
C
      CALL CHCKSI ( 'BFF', BFF, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdas.das' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. Unknown Binary File Format.' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = BIGI3E
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, 2, 'UNKNOWN-', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .FALSE., ' '       )
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNBFF)', OK )
 
C
C     Now check BFF.
C
      CALL CHCKSI ( 'BFF', BFF, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. BFFID: BIG-IEEE DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, 2, 'BIG-IEEE', 'ABCD',
     .                 'EFGH', 'IJKL', .TRUE., .FALSE., ' '      )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', BIGI3E, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. BFFID: LTL-IEEE DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, 2, 'LTL-IEEE', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .FALSE., ' '       )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', LTLI3E, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. BFFID: VAX-GFLT DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, 2, 'VAX-GFLT', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .FALSE., ' '       )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', VAXGFL, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. BFFID: VAX-DFLT DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, 2, 'VAX-DFLT', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .FALSE., ' '       )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', VAXDFL, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. BFFID: BIG-IEEE DAS' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAS
      BFF = 0
 
      CALL KILFIL ( 'testdas.das' )
 
      CALL T_CPTFIL ( 'testdas.das', ARC, 2, 'BIG-IEEE', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .FALSE., ' '       )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdas.das',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', BIGI3E, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdas.das' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. BFFID: LTL-IEEE DAS' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAS
      BFF = 0
 
      CALL KILFIL ( 'testdas.das' )
 
      CALL T_CPTFIL ( 'testdas.das', ARC, 2, 'LTL-IEEE', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .FALSE., ' '       )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdas.das',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', LTLI3E, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdas.das' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. BFFID: VAX-GFLT DAS' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAS
      BFF = 0
 
      CALL KILFIL ( 'testdas.das' )
 
      CALL T_CPTFIL ( 'testdas.das', ARC, 2, 'VAX-GFLT', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .FALSE., ' '       )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdas.das',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', VAXGFL, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdas.das' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. BFFID: VAX-DFLT DAS' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAS
      BFF = 0
 
      CALL KILFIL ( 'testdas.das' )
 
      CALL T_CPTFIL ( 'testdas.das', ARC, 2, 'VAX-DFLT', 'ABCD',
     .                'EFGH', 'IJKL', .TRUE., .FALSE., ' '       )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdas.das',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', VAXDFL, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdas.das' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No FTP String. No BFFID. DAS' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAS
      BFF = 0
 
      CALL KILFIL ( 'testdas.das' )
 
      CALL T_CPTFIL ( 'testdas.das', ARC, 2, '        ', 'ABCD',
     .                'EFGH', 'IJKL', .FALSE., .FALSE., ' '      )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdas.das',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Fetch the native environment from ZZPLATFM.
C
      CALL ZZPLATFM ( 'FILE_FORMAT', NATIVE )
      CALL UCASE    ( NATIVE,        NATIVE )
C
C     Convert it to the appropriate integer code.
C
      NATBFF = ISRCHC ( NATIVE, NUMBFF, STRBFF )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', NATBFF, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdas.das' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No FTP String. No BFFID. BIG-IEEE DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = NULL
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = CHAR(128)
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, 2, '       ', CNI,
     .                'EFGH', 'IJKL', .FALSE., .FALSE., ' '  )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', BIGI3E, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. Null BFFID. BIG-IEEE DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = NULL
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = CHAR(128)
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, 2, NULBFF, CNI,
     .                'EFGH', 'IJKL', .TRUE., .FALSE., ' '  )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', BIGI3E, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No FTP String. No BFFID. LTL-IEEE DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = CHAR(128)
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = NULL
 
C
C     As long as we keep FDREC byte-sized we can do the following.
C
      FDREC       = 12
      CFDREC(1:1) = CHAR(FDREC)
      CFDREC(2:2) = NULL
      CFDREC(3:3) = NULL
      CFDREC(4:4) = NULL
 
C
C     Now construct the character NSUM values.  Recall that the first
C     two bytes must be NULL in order to properly represent LTL-IEEE.
C
      CNSUM(1:1) = NULL
      CNSUM(2:2) = NULL
      CNSUM(3:3) = NULL
      CNSUM(4:4) = NULL
      CNSUM(5:5) = CHAR(128)
      CNSUM(6:6) = NULL
      CNSUM(7:7) = CHAR(64)
      CNSUM(8:8) = CHAR(80)
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, FDREC, '       ',
     .                CNI, CFDREC, CNSUM, .FALSE., .FALSE., ' '  )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', LTLI3E, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. Null BFFID. LTL-IEEE DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = CHAR(128)
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = NULL
 
C
C     As long as we keep FDREC byte-sized we can do the following.
C
      FDREC       = 12
      CFDREC(1:1) = CHAR(FDREC)
      CFDREC(2:2) = NULL
      CFDREC(3:3) = NULL
      CFDREC(4:4) = NULL
 
C
C     Now construct the character NSUM values.  Recall that the first
C     two bytes must be NULL in order to properly represent LTL-IEEE.
C
      CNSUM(1:1) = NULL
      CNSUM(2:2) = NULL
      CNSUM(3:3) = NULL
      CNSUM(4:4) = NULL
      CNSUM(5:5) = CHAR(128)
      CNSUM(6:6) = NULL
      CNSUM(7:7) = CHAR(64)
      CNSUM(8:8) = CHAR(80)
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, FDREC, NULBFF,
     .                CNI, CFDREC, CNSUM, .TRUE., .FALSE., ' '  )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', LTLI3E, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No FTP String. No BFFID. VAX-GFLT DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = CHAR(128)
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = NULL
 
C
C     As long as we keep FDREC byte-sized we can do the following.
C
      FDREC       = 20
      CFDREC(1:1) = CHAR(FDREC)
      CFDREC(2:2) = NULL
      CFDREC(3:3) = NULL
      CFDREC(4:4) = NULL
 
C
C     Now construct the character NSUM values.  Recall that the first
C     two bytes must be NULL in order to properly represent LTL-IEEE.
C
      CNSUM(1:1) = CHAR(16)
      CNSUM(2:2) = CHAR(64)
      CNSUM(3:3) = NULL
      CNSUM(4:4) = CHAR(64)
      CNSUM(5:5) = NULL
      CNSUM(6:6) = NULL
      CNSUM(7:7) = NULL
      CNSUM(8:8) = NULL
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, FDREC, '       ',
     .                CNI, CFDREC, CNSUM, .FALSE., .FALSE., ' '  )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', VAXGFL, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. Null BFFID. VAX-GFLT DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = CHAR(128)
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = NULL
 
C
C     As long as we keep FDREC byte-sized we can do the following.
C
      FDREC       = 20
      CFDREC(1:1) = CHAR(FDREC)
      CFDREC(2:2) = NULL
      CFDREC(3:3) = NULL
      CFDREC(4:4) = NULL
 
C
C     Now construct the character NSUM values.  Recall that the first
C     two bytes must be NULL in order to properly represent LTL-IEEE.
C
      CNSUM(1:1) = CHAR(16)
      CNSUM(2:2) = CHAR(64)
      CNSUM(3:3) = NULL
      CNSUM(4:4) = CHAR(64)
      CNSUM(5:5) = NULL
      CNSUM(6:6) = NULL
      CNSUM(7:7) = NULL
      CNSUM(8:8) = NULL
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, FDREC, NULBFF,
     .                CNI, CFDREC, CNSUM, .TRUE., .FALSE., ' '  )
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', VAXGFL, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No FTP String. No BFFID. VAX-DFLT DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = CHAR(128)
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = NULL
 
C
C     As long as we keep FDREC byte-sized we can do the following.
C
      FDREC       = 20
      CFDREC(1:1) = CHAR(FDREC)
      CFDREC(2:2) = NULL
      CFDREC(3:3) = NULL
      CFDREC(4:4) = NULL
 
C
C     Now construct the character NSUM values.  Recall that the first
C     two bytes must be NULL in order to properly represent LTL-IEEE.
C
      CNSUM(1:1) = NULL
      CNSUM(2:2) = CHAR(65)
      CNSUM(3:3) = NULL
      CNSUM(4:4) = NULL
      CNSUM(5:5) = NULL
      CNSUM(6:6) = NULL
      CNSUM(7:7) = NULL
      CNSUM(8:8) = NULL
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, FDREC, '       ',
     .                CNI, CFDREC, CNSUM, .FALSE., .FALSE., ' ' )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', VAXDFL, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. Null BFFID. VAX-DFLT DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = CHAR(128)
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = NULL
 
C
C     As long as we keep FDREC byte-sized we can do the following.
C
      FDREC       = 20
      CFDREC(1:1) = CHAR(FDREC)
      CFDREC(2:2) = NULL
      CFDREC(3:3) = NULL
      CFDREC(4:4) = NULL
 
C
C     Now construct the character NSUM values.  Recall that the first
C     two bytes must be NULL in order to properly represent LTL-IEEE.
C
      CNSUM(1:1) = NULL
      CNSUM(2:2) = CHAR(65)
      CNSUM(3:3) = NULL
      CNSUM(4:4) = NULL
      CNSUM(5:5) = NULL
      CNSUM(6:6) = NULL
      CNSUM(7:7) = NULL
      CNSUM(8:8) = NULL
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, FDREC, NULBFF,
     .                CNI, CFDREC, CNSUM, .TRUE., .FALSE., ' ' )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', VAXDFL, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No FTP String. No BFFID. VAX-DFLT (Exception) DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = CHAR(128)
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = NULL
 
C
C     As long as we keep FDREC byte-sized we can do the following.
C
      FDREC       = 20
      CFDREC(1:1) = CHAR(FDREC)
      CFDREC(2:2) = NULL
      CFDREC(3:3) = NULL
      CFDREC(4:4) = NULL
 
C
C     Now construct the character NSUM values.  Recall that the first
C     two bytes must be NULL in order to properly represent LTL-IEEE.
C
      CNSUM(1:1) = CHAR(128)
      CNSUM(2:2) = CHAR(64)
      CNSUM(3:3) = NULL
      CNSUM(4:4) = NULL
      CNSUM(5:5) = NULL
      CNSUM(6:6) = NULL
      CNSUM(7:7) = NULL
      CNSUM(8:8) = NULL
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, FDREC, '       ',
     .                CNI, CFDREC, CNSUM, .FALSE., .FALSE., ' '  )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', VAXDFL, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FTP String. Null BFFID. VAX-DFLT (Exception) DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = 0
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = CHAR(128)
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = NULL
 
C
C     As long as we keep FDREC byte-sized we can do the following.
C
      FDREC       = 20
      CFDREC(1:1) = CHAR(FDREC)
      CFDREC(2:2) = NULL
      CFDREC(3:3) = NULL
      CFDREC(4:4) = NULL
 
C
C     Now construct the character NSUM values.  Recall that the first
C     two bytes must be NULL in order to properly represent LTL-IEEE.
C
      CNSUM(1:1) = CHAR(128)
      CNSUM(2:2) = CHAR(64)
      CNSUM(3:3) = NULL
      CNSUM(4:4) = NULL
      CNSUM(5:5) = NULL
      CNSUM(6:6) = NULL
      CNSUM(7:7) = NULL
      CNSUM(8:8) = NULL
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, FDREC, NULBFF,
     .                CNI, CFDREC, CNSUM, .TRUE., .FALSE., ' '  )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', VAXDFL, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No FTP String. No BFFID. Zero-Sized DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = -1
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = CHAR(128)
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = NULL
 
C
C     As long as we keep FDREC byte-sized we can do the following.
C
      FDREC       = 20
      CFDREC(1:1) = CHAR(FDREC)
      CFDREC(2:2) = NULL
      CFDREC(3:3) = NULL
      CFDREC(4:4) = NULL
 
C
C     Now construct the character NSUM values.  Recall that the first
C     two bytes must be NULL in order to properly represent LTL-IEEE.
C
      CNSUM(1:1) = NULL
      CNSUM(2:2) = NULL
      CNSUM(3:3) = NULL
      CNSUM(4:4) = NULL
      CNSUM(5:5) = NULL
      CNSUM(6:6) = NULL
      CNSUM(7:7) = NULL
      CNSUM(8:8) = NULL
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, FDREC, '       ',
     .                CNI, CFDREC, CNSUM, .FALSE., .FALSE., ' ' )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNBFF)', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No FTP String. No BFFID. Fall-through ARCH DAF' )
 
C
C     Setup the inputs and outputs.
C
      CALL GETLUN ( UNIT )
 
      ARC = DAF
      BFF = -1
 
      CALL KILFIL ( 'testdaf.daf' )
 
      CNI(1:1) = CHAR(128)
      CNI(2:2) = NULL
      CNI(3:3) = NULL
      CNI(4:4) = NULL
 
C
C     As long as we keep FDREC byte-sized we can do the following.
C
      FDREC       = 20
      CFDREC(1:1) = CHAR(FDREC)
      CFDREC(2:2) = NULL
      CFDREC(3:3) = NULL
      CFDREC(4:4) = NULL
 
C
C     Now construct the character NSUM values.  Recall that the first
C     two bytes must be NULL in order to properly represent LTL-IEEE.
C
      CNSUM(1:1) = CHAR(144)
      CNSUM(2:2) = CHAR(64)
      CNSUM(3:3) = NULL
      CNSUM(4:4) = NULL
      CNSUM(5:5) = NULL
      CNSUM(6:6) = NULL
      CNSUM(7:7) = NULL
      CNSUM(8:8) = NULL
 
      CALL T_CPTFIL ( 'testdaf.daf', ARC, FDREC, '       ',
     .                CNI, CFDREC, CNSUM, .FALSE., .FALSE., ' ' )
 
 
      OPEN ( UNIT   = UNIT,
     .       FILE   = 'testdaf.daf',
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'OLD',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C     Now invoke the module.
C
      CALL ZZDDHPPF ( UNIT, ARC, BFF )
 
C
C     Check for the absence of an exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNBFF)', OK )
 
C
C     Check BFF for the appropriate value.
C
      CALL CHCKSI ( 'BFF', BFF, '=', 0, 0, OK )
 
C
C     Close and remove the file.
C
      CLOSE ( UNIT = UNIT )
 
      CALL KILFIL ( 'testdaf.daf' )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
