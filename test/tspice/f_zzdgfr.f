C$Procedure F_ZZDGFR ( ZZDAFGFR Test Family )

      SUBROUTINE F_ZZDGFR ( OK )

C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDAFGFR
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
C     This routine exercises ZZDAFGFR's logic.
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
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    TSPICE Version 2.1.0, 11-OCT-2021 (NJB)
C
C        Bug fix: in test case for invalid handle, changed index
C        of HANLST from I to 1.
C
C        Reordered header sections.
C
C-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
C
C        Updated module as the result of changes to the handle
C        manager interface, ZZDDHCLS.
C
C-    TSPICE Version 1.0.0, 15-OCT-2001 (FST)
C
C
C-&

C
C     Local Variables
C
      CHARACTER*(80)        CASENM
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMTMP
      CHARACTER*(8)         IDWORD
      CHARACTER*(60)        IFNAME
      CHARACTER*(60)        RDIFN
      CHARACTER*(8)         RDIDW
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )

      INTEGER               BWARD
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               HANLST ( NUMBFF )
      INTEGER               I
      INTEGER               NATBFF
      INTEGER               ND
      INTEGER               NI
      INTEGER               NUMSUP
      INTEGER               RDBWD
      INTEGER               RDFRE
      INTEGER               RDFWD
      INTEGER               RDND
      INTEGER               RDNI
      INTEGER               SUPBFF ( NUMBFF )
      INTEGER               UNIT

      LOGICAL               ADDFTP
      LOGICAL               FOUND

C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_ZZDGFR' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'F_ZZDGFR Initialization' )

C
C     Retrieve the native format and other related information.
C
      CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, STRAMH, STRARC, STRBFF )

C
C     Setup the filename template.
C
      FNMTMP = 'daf#.daf'

C
C     Construct the contents of the DAF to create.
C
      IDWORD = 'DAF/TEST'
      ND     = 12
      NI     = 16
      IFNAME = 'TSPICE Test DAF Striving for 60 Characters Going Once'
     .//       ' Twice.'
      FWARD  = -4
      BWARD  = 12
      FREE   = 4*128+1
      ADDFTP = .TRUE.

C
C     Construct the DAFs for each environment and load them
C     into the handle manager.
C
      DO I = 1, NUMSUP

         CALL REPMI    ( FNMTMP, '#', I, FNAME )

C
C        Start a new DAF.
C
         CALL T_DAFOPN ( FNAME, SUPBFF(I), UNIT )

C
C        Dump the file record to it.
C
         CALL T_DAFWFR ( UNIT,  SUPBFF(I), IDWORD, ND,    NI, IFNAME,
     .                   FWARD, BWARD,     FREE,   ADDFTP             )

C
C        Close the file.  Since we added the FTP string, ZZDDHOPN
C        only requires a file record to open the file.
C
         CLOSE ( UNIT = UNIT )

C
C        Open the DAF in the handle manager.
C
         CALL ZZDDHOPN ( FNAME, 'READ', 'DAF', HANLST(I) )

C
C        Check for the absence of an exception.
C
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Dynamically construct the test cases based on the contents
C     of SUPBFF.
C
      DO I = 1, NUMSUP

         CASENM = '# reading # file record data.'
         CALL REPMC ( CASENM, '#', STRBFF(NATBFF),    CASENM )
         CALL REPMC ( CASENM, '#', STRBFF(SUPBFF(I)), CASENM )

         CALL TCASE ( CASENM )

C
C        Setup the inputs and outputs.
C
         RDND  = 0
         RDNI  = 0
         RDIDW = ' '
         RDIFN = ' '
         RDFWD = 0
         RDBWD = 0
         RDFRE = 0

         FOUND = .FALSE.

C
C        We have all the pieces, invoke the module.
C
         CALL ZZDAFGFR ( HANLST(I),
     .                   RDIDW,
     .                   RDND,
     .                   RDNI,
     .                   RDIFN,
     .                   RDFWD,
     .                   RDBWD,
     .                   RDFRE,
     .                   FOUND      )

C
C        Check for the absence of an exception.
C
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check outputs.
C
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         CALL CHCKSC ( 'IDWORD', RDIDW, '=', IDWORD, OK )

         CALL CHCKSI ( 'ND', RDND, '=', ND, 0, OK )
         CALL CHCKSI ( 'NI', RDNI, '=', NI, 0, OK )

         CALL CHCKSC ( 'IFNAME', RDIFN, '=', IFNAME, OK )

         CALL CHCKSI ( 'FWARD', RDFWD, '=', FWARD, 0, OK )
         CALL CHCKSI ( 'BWARD', RDBWD, '=', BWARD, 0, OK )
         CALL CHCKSI ( 'FREE',  RDFRE, '=', FREE,  0, OK )

      END DO

C
C     Clean up by unloading and removing the test files.
C
      DO I = 1, NUMSUP
         CALL ZZDDHCLS ( HANLST(I), 'DAF', .FALSE. )
         CALL REPMI  ( FNMTMP, '#', I, FNAME )
         CALL KILFIL ( FNAME )
      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPICE(HANDLENOTFOUND) Exception' )

C
C     Setup outputs.
C
      FOUND = .TRUE.

      RDIDW = ' '
      RDND  = 0
      RDNI  = 0
      RDIFN = ' '
      RDFWD = 0
      RDBWD = 0
      RDFRE = 0

C
C     Since we know we just unloaded HANLST(1), attempt to read
C     from that handle.
C
      CALL ZZDAFGFR ( HANLST(1),
     .                RDIDW,
     .                RDND,
     .                RDNI,
     .                RDIFN,
     .                RDFWD,
     .                RDBWD,
     .                RDFRE,
     .                FOUND      )


C
C     Check for the presence of an exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(HANDLENOTFOUND)', OK )

C
C     Check outputs. FOUND should be FALSE, and RDREC should be
C     untouched.
C
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL CHCKSC ( 'IDWORD', RDIDW, '=', ' ', OK )

      CALL CHCKSI ( 'ND', RDND, '=', 0, 0, OK )
      CALL CHCKSI ( 'NI', RDNI, '=', 0, 0, OK )

      CALL CHCKSC ( 'IFNAME', RDIFN, '=', ' ', OK )

      CALL CHCKSI ( 'FWARD', RDFWD, '=', 0, 0, OK )
      CALL CHCKSI ( 'BWARD', RDBWD, '=', 0, 0, OK )
      CALL CHCKSI ( 'FREE',  RDFRE, '=', 0,  0, OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN

      END
