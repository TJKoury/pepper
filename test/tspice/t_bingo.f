C$Procedure T_BINGO ( BINGO: Process binary kernels to alternate BFFs )
 
      SUBROUTINE T_BINGO ( INAME, ONAME, OBFF )
 
C$ Abstract
C
C     Convert SPICE binary kernels from one supported binary file
C     format to another.
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
C     DAF
C     DAS
C
C$ Keywords
C
C     TEST ROUTINE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      CHARACTER*(*)         INAME
      CHARACTER*(*)         ONAME
      INTEGER               OBFF
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INAME      I   Name of source binary kernel to convert.
C     ONAME      I   Name of output binary kernel to create.
C     OBFF       I   Integer code for binary file format of ONAME.
C
C$ Detailed_Input
C
C     INAME      is the name of the binary kernel to convert to
C                an alternate binary file format.
C
C     ONAME      is the name of the converted binary kernel to create.
C                The file named by ONAME will be destroyed and
C                replaced with the converted file.
C
C     OBFF       is an integer code that indicates the binary file
C                format targeted for ONAME.  Acceptable values are
C                the parameters:
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
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the output file is already known to the handle manager
C         subsystem, the error SPICE(CANTCREATEFILE) is signaled.
C
C     2)  If the input file can't be opened, the error 
C         SPICE(FILEOPENFAILED) is signaled.
C
C     3)  If the ID word of the input file can't be read, the error 
C         SPICE(FILEREADFAILED) is signaled.
C
C     4)  If the input file's architecture is neither DAF nor DAS,
C         the error SPICE(UNKNOWNARCH) is signaled.
C         
C     5)  Errors detected while converting the input file will be
C         diagnosed by routines in the call tree of this routine.
C
C$ Files
C
C     This routine opens the file named by INAME for read access, and
C     creates (destroying any pre-existing file if necessary) the
C     file named by ONAME.
C
C$ Particulars
C
C     This test routine allows existing test software that creates
C     DAFs using the high level writers to create native format
C     files and convert them to supported non-native formats formats
C     for testing purposes.
C
C     As new binary file formats for existing kernels are added to
C     the list of those officially supported, this routine and
C     the routines it calls may require significant updates.
C
C$ Examples
C
C     See the TSPICE routine T_DAFNN for an example of usage.
C
C$ Restrictions
C
C     This code is cobbled together in a rather non-robust way.
C     Do not attempt to convert binary kernels that are not 100%
C     correct in their structure and composition.  Using this
C     routine on files with questionable construction will yield
C     unpredictable results at best.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    TSPICE Version 2.0.0, 11-FEB-2015 (NJB)
C
C        Updated to support DAS conversion.
C
C        Filled in header Exceptions section.
C
C-    SPICELIB Version 1.0.0, 28-NOV-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
 
      LOGICAL               EQSTR
      LOGICAL               RETURN
 
C
C     Local Variables
C
      CHARACTER*(8)         ARCH
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(8)         IDWORD
      CHARACTER*(8)         TYPE
 
 
      INTEGER               HANDLE
      INTEGER               IAMH
      INTEGER               IARC
      INTEGER               IBFF
      INTEGER               IOSTAT
      INTEGER               UNIT
 
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_BINGO' )
 
C
C     First things first, check to see if the handle manager has
C     an entry for ONAME.
C
      CALL ZZDDHFNH ( ONAME, HANDLE, FOUND )
 
      IF ( FOUND ) THEN
 
         CALL SETMSG ( 'Can not create file, ''#''.  It is currently '
     .   //            'loaded and associated with handle, #.  This '
     .   //            'routine may only create files not currently '
     .   //            'loaded.'                                       )
         CALL ERRCH  ( '#', ONAME                                      )
         CALL ERRINT ( '#', HANDLE                                     )
         CALL SIGERR ( 'SPICE(CANTCREATEFILE)'                         )
         CALL CHKOUT ( 'T_BINGO'                                       )
         RETURN
 
      END IF
 
C
C     Now, see if IFNAME is loaded in the handle manager.
C
      CALL ZZDDHFNH ( INAME, HANDLE, FOUND )
 
C
C     If it is loaded, then use the value of IARC to indicate the
C     file architecture.
C
      IF ( FOUND ) THEN
 
         CALL ZZDDHNFO ( HANDLE, FNAME, IARC, IBFF, IAMH, FOUND )
 
C
C     Otherwise, open the file and extract the 8 character IDWORD.
C
      ELSE
 
         CALL GETLUN ( UNIT )
 
         OPEN ( UNIT   = UNIT,
     .          FILE   = INAME( 1 : RTRIM(INAME) ),
     .          ACCESS = 'DIRECT',
     .          RECL   = RECL,
     .          STATUS = 'OLD',
     .          IOSTAT = IOSTAT                     )
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to open file, ''#''. IOSTAT = #.' )
            CALL ERRCH  ( '#', INAME                                )
            CALL ERRINT ( '#', IOSTAT                               )
            CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                   )
            CALL CHKOUT ( 'T_BINGO'                                 )
            RETURN
         END IF
 
         READ ( UNIT   = UNIT,
     .          REC    = 1,
     .          IOSTAT = IOSTAT ) IDWORD
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to read ID word from file, '
     .      //            '''#''.  IOSTAT = #.'                )
            CALL ERRCH  ( '#', INAME                           )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'              )
            CALL CHKOUT ( 'T_BINGO'                            )
            RETURN
         END IF
 
         CALL IDW2AT ( IDWORD, ARCH, TYPE )
 
         IF ( EQSTR( ARCH, 'DAF' ) ) THEN
            IARC = DAF
         ELSE IF ( EQSTR( ARCH, 'DAS' ) ) THEN
            IARC = DAS
         ELSE
            IARC = 0
         END IF
 
         CLOSE ( UNIT = UNIT )
 
      END IF
 
C
C     IARC is set properly, branch appropriately.
C
      IF ( IARC .EQ. DAF ) THEN
 
         CALL T_BGODAF ( INAME, ONAME, OBFF )
 
      ELSE IF ( IARC .EQ. DAS ) THEN
 
         CALL T_BGODAS ( INAME, ONAME, OBFF )
 
      ELSE
 
         CALL SETMSG ( 'The architecture of file, ''#'', is '
     .   //            'unknown.  No conversion possible.'    )
         CALL SIGERR ( 'SPICE(UNKNOWNARCH)'                   )
         CALL CHKOUT ( 'T_BINGO'                              )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'T_BINGO' )
      RETURN
 
      END
