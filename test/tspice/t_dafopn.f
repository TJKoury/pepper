C$Procedure T_DAFOPN ( Open a new DAF file for testing )
 
      SUBROUTINE T_DAFOPN ( FNAME, OUTBFF, UNIT )
 
C$ Abstract
C
C     Open a new DAF file.
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
C     TEST ROUTINE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      CHARACTER*(*)         FNAME
      INTEGER               OUTBFF
      INTEGER               UNIT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     FNAME      I   Name of the DAF to open.
C     OUTBFF     I   Binary file format code used to create file.
C     UNIT       O   Logical unit used to open the file.
C
C$ Detailed_Input
C
C     FNAME      is the name of the DAF that is to be created.
C
C     OUTBFF     is an integer code that indicates the binary file
C                format targeted for OUTPUT.  Acceptable values
C                are the parameters:
C
C                   BIGI3E
C                   LTLI3E
C                   VAXGFL
C                   VAXDFL
C
C                as defined in the include file 'zzddhman.inc'.
C
C$ Detailed_Output
C
C     UNIT       is the logical unit used to open FNAME.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     This module opens FNAME with NEW status.
C
C$ Exceptions
C
C     1) SPICE(FILEOPENFAILED) is signaled if the OPEN statement fails.
C
C$ Particulars
C
C     This routine simply opens a new DAF and returns the logical
C     unit associated with the DAF.
C
C     Note: This code is not a model of perfection.  It is hacked
C     together from pieces of code written elsewhere to support
C     creation of non-native binaries for testing purposes only.
C
C$ Examples
C
C     See F_ZZDGDR for usage.
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
C-    SPICELIB Version 1.0.0, 28-OCT-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
      LOGICAL               RETURN
 
C
C     Local Variables
C
      INTEGER               IOSTAT
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_DAFOPN' )
      END IF
 
C
C     First kill FNAME.
C
      CALL KILFIL ( FNAME )
 
C
C     Now fetch a logical unit.
C
      CALL GETLUN ( UNIT )
 
C
C     Open the file.
C
      OPEN ( UNIT   = UNIT,
     .       FILE   = FNAME(1:RTRIM(FNAME)),
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'NEW',
     .       IOSTAT = IOSTAT                 )
 
C
C     Check IOSTAT.
C
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Unable to open file, #.  IOSTAT was #.' )
         CALL ERRCH  ( '#', FNAME                               )
         CALL ERRINT ( '#', IOSTAT                              )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                  )
         CALL CHKOUT ( 'T_DAFOPN'                               )
         RETURN
      END IF
 
      CALL CHKOUT ( 'T_DAFOPN' )
      RETURN
 
      END
