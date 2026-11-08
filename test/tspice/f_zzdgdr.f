C$Procedure F_ZZDGDR ( ZZDAFGDR Test Family )
 
      SUBROUTINE F_ZZDGDR ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDAFGDR
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
C     This routine exercises ZZDAFGDR's logic.
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
C-    TSPICE Version 2.26.0, 28-NOV-2021 (BVS)
C
C        Updated for MAC-OSX-M1-64BIT-CLANG_C.
C
C-    TSPICE Version 2.25.0, 10-MAR-2014 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-INTEL.
C
C-    TSPICE Version 2.24.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-LINUX-64BIT-IFORT.
C
C-    TSPICE Version 2.23.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-GFORTRAN.
C
C-    TSPICE Version 2.22.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GFORTRAN.
C
C-    TSPICE Version 2.21.0, 10-MAR-2014 (BVS)
C
C        Updated for PC-CYGWIN-64BIT-GCC_C.
C
C-    TSPICE Version 2.20.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL.
C
C-    TSPICE Version 2.19.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-CC_C.
C
C-    TSPICE Version 2.18.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
C
C-    TSPICE Version 2.17.0, 13-MAY-2010 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
C
C-    TSPICE Version 2.16.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-WINDOWS-64BIT-IFORT.
C
C-    TSPICE Version 2.15.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-LINUX-64BIT-GFORTRAN.
C
C-    TSPICE Version 2.14.0, 13-MAY-2010 (BVS)
C
C        Updated for PC-64BIT-MS_C.
C
C-    TSPICE Version 2.13.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-INTEL_C.
C
C-    TSPICE Version 2.12.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-IFORT.
C
C-    TSPICE Version 2.11.0, 13-MAY-2010 (BVS)
C
C        Updated for MAC-OSX-64BIT-GFORTRAN.
C
C-    TSPICE Version 2.10.0, 18-MAR-2009 (BVS)
C
C        Updated for PC-LINUX-GFORTRAN.
C
C-    TSPICE Version 2.9.0, 18-MAR-2009 (BVS)
C
C        Updated for MAC-OSX-GFORTRAN.
C
C-    TSPICE Version 2.8.0, 19-FEB-2008 (BVS)
C
C        Updated for PC-LINUX-IFORT.
C
C-    TSPICE Version 2.7.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-LINUX-64BIT-GCC_C.
C
C-    TSPICE Version 2.6.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-INTEL_C.
C
C-    TSPICE Version 2.5.0, 14-NOV-2006 (BVS)
C
C        Updated for MAC-OSX-IFORT.
C
C-    TSPICE Version 2.4.0, 14-NOV-2006 (BVS)
C
C        Updated for PC-WINDOWS-IFORT.
C
C-    TSPICE Version 2.3.0, 26-OCT-2005 (BVS)
C
C        Updated for SUN-SOLARIS-64BIT-GCC_C.
C
C-    TSPICE Version 2.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN_C.
C
C-    TSPICE Version 2.1.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
C
C        Updated module as the result of changes to the handle
C        manager interface, ZZDDHCLS.
C
C        Added MAC OS/X environments.
C
C-    TSPICE Version 1.0.0, 15-OCT-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
 
C
C     Local Parameters
C
 
C
C     Local Variables
C
      CHARACTER*(80)        CASENM
      CHARACTER*(1024)      NAME
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMTMP
      CHARACTER*(8)         IDWORD
      CHARACTER*(60)        IFNAME
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
 
      DOUBLE PRECISION      ARRAY  (  128   )
      DOUBLE PRECISION      DATA   (  128   )
      DOUBLE PRECISION      DC     (  125   )
      DOUBLE PRECISION      RDREC  (  128   )
 
      INTEGER               BWARD
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               HANLST ( NUMBFF )
      INTEGER               I
      INTEGER               IC     (  250   )
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               NATBFF
      INTEGER               ND
      INTEGER               NEXT
      INTEGER               NI
      INTEGER               NSUM
      INTEGER               NUMSUP
      INTEGER               PREV
      INTEGER               SUPBFF ( NUMBFF )
      INTEGER               UNIT
 
      LOGICAL               ADDFTP
      LOGICAL               FOUND
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_ZZDGDR' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'F_ZZDGDR Initialization' )
 
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
      ND     = 2
      NI     = 6
      IFNAME = 'TSPICE Test DAF'
      FWARD  = 2
      BWARD  = 2
      FREE   = 4*128+1
      ADDFTP = .FALSE.
      NEXT   = 0
      PREV   = 0
      NSUM   = 2
 
C
C     Create and pack the summaries.
C
      DC(1) = 1.0D0
      DC(2) = -1.0D0
 
      IC(1) = 1
      IC(2) = 2
      IC(3) = 3
      IC(4) = 4
      IC(5) = 3*128+1
      IC(6) = IC(5) + 64
 
      CALL DAFPS ( ND, NI, DC, IC, ARRAY )
 
      DC(1) = 2.0D0
      DC(2) = -2.0D0
 
      IC(1) = 2
      IC(2) = 4
      IC(3) = 6
      IC(4) = 8
      IC(5) = IC(6) + 1
      IC(6) = 4*128
 
      CALL DAFPS ( ND, NI, DC, IC, ARRAY( (ND+(NI+1)/2) + 1 ) )
 
C
C     Setup the name record.
C
      NAME = ' '
 
C
C     Populate the data record with a simple pattern.  Our
C     aim is not to validate the translation algorithm, but
C     the reader module.
C
      DO I = 1, 128
         DATA(I) = 10.0D0**I
      END DO
 
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
C        Now write the only summary record to the file.
C
         CALL T_DAFWSR ( UNIT, 2, SUPBFF(I), ND, NI, NEXT, PREV, NSUM,
     .                   ARRAY                                      )
 
C
C        Insert a blank name record for completeness.
C
         WRITE ( UNIT = UNIT, REC = 3, IOSTAT = IOSTAT ) NAME
         CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )
 
C
C        Lastly, dump the data record and close the file.
C
         CALL T_DAFWDR ( UNIT, 4, SUPBFF(I), 128, DATA )
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
 
         CASENM = '# reading # data.'
         CALL REPMC ( CASENM, '#', STRBFF(NATBFF),    CASENM )
         CALL REPMC ( CASENM, '#', STRBFF(SUPBFF(I)), CASENM )
 
         CALL TCASE ( CASENM )
 
C
C        Setup the inputs and outputs.
C
         DO J = 1, 128
            RDREC(J) = 0.0D0
         END DO
 
         FOUND = .FALSE.
 
C
C        We have all the pieces, invoke the module.
C
         CALL ZZDAFGDR ( HANLST(I), 4, RDREC, FOUND )
 
C
C        Check for the absence of an exception.
C
         CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C        Check outputs.
C
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
         CALL CHCKAD ( 'RDREC', RDREC, '=', DATA, 128, 0.0D0, OK )
 
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
 
      DO I = 1, 128
         RDREC(I) = DATA(I)
      END DO
 
C
C     Since we know we just unloaded HANLST(1), attempt to read
C     from that handle.
C
      CALL ZZDAFGDR ( HANLST(1), 4, RDREC, FOUND )
 
C
C     Check for the presence of an exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(HANDLENOTFOUND)', OK )
 
C
C     Check outputs. FOUND should be FALSE, and RDREC should be
C     untouched.
C
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKAD ( 'RDREC', RDREC, '=', DATA, 128, 0.0D0, OK )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
