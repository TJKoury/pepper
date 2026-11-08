C$Procedure F_DASFR ( DASFR Test Family )
 
      SUBROUTINE F_DASFR ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the DASFR
C     umbrella.
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
C     This routine exercises a few simple test cases that verify the
C     FTP validation string and the binary file format strings were
C     added properly to DAS files.
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
C-    TSPICE Version 1.0.0, 11-DEC-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
 
C
C     Local Parameters
C
      INTEGER               FMTLEN
      PARAMETER           ( FMTLEN = 8 )
 
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
 
C
C     See DASWFR for a detailed explanation of TAILEN.
C
      INTEGER               TAILEN
      PARAMETER           ( TAILEN = 932 )
 
C
C     Local Variables
C
      CHARACTER*(FMTLEN)    FORMAT
      CHARACTER*(FMTLEN)    SYSFMT
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(TAILEN)    TAIL
 
      INTEGER               HANDLE
      INTEGER               IOSTAT
      INTEGER               LUN
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC
 
      LOGICAL               FTPERR
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_DASFR' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDASNFR Test' )
 
C
C     Setup the file.
C
      FNAME = 'test.das'
      CALL KILFIL ( FNAME )
 
      CALL GETLUN ( LUN )
 
      OPEN ( UNIT   = LUN,
     .       FILE   = FNAME(1:RTRIM(FNAME)),
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'NEW',
     .       IOSTAT = IOSTAT           )
 
      CALL ZZDASNFR ( LUN, 'DAS/TEST', 'TEST DAS', -1, 1, -2, 2,
     .                'BFFIDWRD'                                  )
 
C
C     Initialize the values we are about to read.
C
      IDWORD = ' '
      IFNAME = ' '
      NRESVR = 0
      NRESVC = 0
      NCOMR  = 0
      NCOMC  = 0
      FORMAT = ' '
      TAIL   = ' '
 
      READ ( UNIT   = LUN,
     .       REC    = 1,
     .       IOSTAT = IOSTAT ) IDWORD,
     .                         IFNAME,
     .                         NRESVR,
     .                         NRESVC,
     .                         NCOMR,
     .                         NCOMC,
     .                         FORMAT,
     .                         TAIL
 
C
C     Check the results.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=',  0, 0,      OK )
      CALL CHCKSC ( 'IDWORD', IDWORD, '=', 'DAS/TEST', OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', 'TEST DAS', OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=', -1, 0,      OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=',  1, 0,      OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=', -2, 0,      OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=',  2, 0,      OK )
      CALL CHCKSC ( 'FORMAT', FORMAT, '=', 'BFFIDWRD', OK )
 
      CALL ZZFTPCHK ( TAIL, FTPERR )
 
      CALL CHCKSL ( 'FTPERR', FTPERR, .FALSE., OK )
 
      CALL CHCKSC ( 'FTPSTR', TAIL(608:613), '=', 'FTPSTR', OK )
 
      CLOSE ( UNIT = LUN )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DASOPN Test' )
 
      FNAME = 'test.das'
 
      CALL KILFIL ( FNAME )
 
      CALL DASOPN ( FNAME, 'TEST DASOPN', HANDLE )
 
      CALL DASRFR ( HANDLE, IDWORD, IFNAME, NRESVR, NRESVC,
     .              NCOMR,  NCOMC                           )
 
C
C     Check the results.  We don't need to worry about FORMAT or
C     the FTP string since DASOPN invokes ZZDASNFR which we just
C     checked.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSC ( 'IDWORD', IDWORD, '=', 'NAIF/DAS',    OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', 'TEST DASOPN', OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=',  0, 0,         OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=',  0, 0,         OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=',  0, 0,         OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=',  0, 0,         OK )
 
      CALL DASCLS ( HANDLE )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DASONW Test' )
 
      FNAME = 'test.das'
 
      CALL KILFIL ( FNAME )
 
      CALL DASONW ( FNAME, 'TEST', 'TEST DASONW', 5, HANDLE )
 
      CALL DASRFR ( HANDLE, IDWORD, IFNAME, NRESVR, NRESVC,
     .              NCOMR,  NCOMC                           )
 
C
C     Check the results.  We don't need to worry about FORMAT or
C     the FTP string since DASONW invokes ZZDASNFR which we just
C     checked.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSC ( 'IDWORD', IDWORD, '=', 'DAS/TEST',    OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', 'TEST DASONW', OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=',  0, 0,         OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=',  0, 0,         OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=',  5, 0,         OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=',  0, 0,         OK )
 
      CALL DASCLS ( HANDLE )
 
      CALL KILFIL ( FNAME )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'DASWFR Test' )
 
      FNAME = 'test.das'
 
      CALL KILFIL ( FNAME )
 
      CALL DASONW ( FNAME, 'TEST', 'TEST DASWFR', 5, HANDLE )
 
      CALL DASWFR ( HANDLE, 'DAS/WORD', 'DASWFR UPDATE', 2, 4,
     .              6, 8                                       )
 
C
C     Check the results.
C
      CALL DASRFR ( HANDLE, IDWORD, IFNAME, NRESVR, NRESVC,
     .              NCOMR,  NCOMC                           )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSC ( 'IDWORD', IDWORD, '=', 'DAS/WORD',      OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', 'DASWFR UPDATE', OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=',  2, 0,           OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=',  4, 0,           OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=',  6, 0,           OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=',  8, 0,           OK )
 
C
C     Check that DASWFR perserved the binary file format and FTP
C     strings.
C
      CALL DASHLU ( HANDLE, LUN )
 
      READ ( UNIT   = LUN,
     .       REC    = 1,
     .       IOSTAT = IOSTAT ) IDWORD,
     .                         IFNAME,
     .                         NRESVR,
     .                         NRESVC,
     .                         NCOMR,
     .                         NCOMC,
     .                         FORMAT,
     .                         TAIL
 
C
C     Check the results.
C
      CALL CHCKSI ( 'IOSTAT', IOSTAT, '=',  0, 0,      OK )
 
      CALL ZZPLATFM ( 'FILE_FORMAT', SYSFMT )
      CALL CHCKSC ( 'FORMAT', FORMAT, '=', SYSFMT, OK )
 
      CALL ZZFTPCHK ( TAIL, FTPERR )
      CALL CHCKSL ( 'FTPERR', FTPERR, .FALSE., OK )
      CALL CHCKSC ( 'FTPSTR', TAIL(608:613), '=', 'FTPSTR', OK )
 
      CALL DASCLS ( HANDLE )
 
      CALL KILFIL ( FNAME )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
