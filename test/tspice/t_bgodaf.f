C$Procedure T_BGODAF ( BINGO: Process DAF files to alternate BFFs )
 
      SUBROUTINE T_BGODAF ( INAME, ONAME, OBFF )
 
C$ Abstract
C
C     Convert DAFs from one supported binary file format to another.
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
 
      CHARACTER*(*)         INAME
      CHARACTER*(*)         ONAME
      INTEGER               OBFF
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INAME      I   Name of source file to convert.
C     ONAME      I   Name of output file to create.
C     OBFF       I   Integer code for binary file format of ONAME.
C
C$ Detailed_Input
C
C     INAME      is the name of the DAF file to convert to an
C                alternate binary file format.
C
C     ONAME      is the name of the converted DAF file to create. The
C                file named by ONAME will be destroyed and replaced
C                with the converted DAF.
C
C     OBFF       is an integer code that indicates the binary file
C                format targeted for ONAME.  Acceptable values
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
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     This routine opens the file named by INAME for read access and
C     creates the file named by ONAME.
C
C$ Exceptions
C
C     1) SPICE(DAFREADFAILURE) is signaled whenever reading records
C        from INAME fails.  In the event such an error occurs,
C        ONAME is deleted.
C
C     2) SPICE(DAFWRITEFAILURE) is signaled whenever writing records
C        to ONAME fails.  In the event such an error occurs, ONAME
C        is deleted.
C
C     3) Routines in the call tree of this routine may signal errors
C        as a result of improper inputs or other exceptional cases.
C
C$ Particulars
C
C      This test routine allows existing test software that creates
C      DAFs using the high level writers to create native format
C      files and convert them to non-native format for testing
C      purposes.
C
C      As new binary file formats are added to the list of those
C      supported, this routine and routines it calls may require
C      updates.
C
C$ Examples
C
C     See some TBD test routine for usage.
C
C$ Restrictions
C
C     1) ONAME must reference a file currently not connected to
C        a unit.  Unpredictable behavior may result otherwise.
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
C-    SPICELIB Version 1.0.0, 28-NOV-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Variables
C
      CHARACTER*(1024)      CREC
      CHARACTER*(8)         IDWORD
      CHARACTER*(60)        IFNAME
 
      DOUBLE PRECISION      DPREC ( 128 )
 
      INTEGER               BWARD
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               I
      INTEGER               INHAN
      INTEGER               INUNIT
      INTEGER               IOSTAT
      INTEGER               ND
      INTEGER               NI
      INTEGER               NXTDSC
      INTEGER               STPREC
      INTEGER               UNIT
      INTEGER               WORD
      LOGICAL               FOUND
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_BGODAF' )
      END IF
 
C
C     Open INAME for read access.
C
      CALL DAFOPR ( INAME, INHAN )
 
C
C     Create ONAME.
C
      CALL T_DAFOPN ( ONAME, OBFF, UNIT )
 
C
C     Read INAME's file record, we need everything so use the very low
C     level reader.
C
      CALL ZZDAFGFR ( INHAN, IDWORD, ND, NI, IFNAME, FWARD, BWARD,
     .                FREE,  FOUND                                 )
 
      IF ( .NOT. FOUND ) THEN
         CALL DAFCLS ( INHAN                         )
         CALL KILFIL ( ONAME                         )
         CALL SETMSG ( 'Unable to find file record.' )
         CALL SIGERR ( 'SPICE(DAFREADFAILURE)'       )
         CALL CHKOUT ( 'T_BGODAF'                    )
         RETURN
      END IF
 
C
C     Dump the file record into ONAME.  We add the FTP string
C     regardless... all new toolkits create files with the FTP
C     string.
C
      CALL T_DAFWFR ( UNIT, OBFF, IDWORD, ND, NI, IFNAME, FWARD,
     .                BWARD, FREE, .TRUE. )
 
C
C     Now just copy all the records between the file record and
C     FWARD.  This is valid at the moment for all supported
C     platforms, but may need to change in the future.
C
      CALL ZZDDHHLU ( INHAN, 'DAF', .FALSE., INUNIT )
 
      DO I = 2, (FWARD-1)
 
         READ ( UNIT   = INUNIT,
     .          REC    = I,
     .          IOSTAT = IOSTAT ) CREC
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL DAFCLS ( INHAN                               )
            CALL KILFIL ( ONAME                               )
            CALL SETMSG ( 'Unable to read comment record, #.' )
            CALL ERRINT ( '#', I                              )
            CALL SIGERR ( 'SPICE(DAFREADFAILURE)'             )
            CALL CHKOUT ( 'T_BGODAF'                          )
            RETURN
         END IF
 
         WRITE ( UNIT   = UNIT,
     .           REC    = I,
     .           IOSTAT = IOSTAT ) CREC
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL DAFCLS ( INHAN                                )
            CALL KILFIL ( ONAME                                )
            CALL SETMSG ( 'Unable to write comment record, #.' )
            CALL ERRINT ( '#', I                               )
            CALL SIGERR ( 'SPICE(DAFWRITEFAILURE)'             )
            CALL CHKOUT ( 'T_BGODAF'                           )
            RETURN
         END IF
 
      END DO
 
C
C     Now process the summary, name, and data records.  The record
C     FREE points to is the last record to be processed.
C
      CALL DAFARW ( FREE, STPREC, WORD )
 
      I      = FWARD
      NXTDSC = FWARD
 
      DO WHILE ( I .LT. STPREC )
 
C
C        See if the record we are currently processing is a
C        descriptor record.
C
         IF ( I .EQ. NXTDSC ) THEN
 
            CALL ZZDAFGSR ( INHAN, I, ND, NI, DPREC, FOUND )
 
            IF ( .NOT. FOUND ) THEN
               CALL DAFCLS ( INHAN                               )
               CALL KILFIL ( ONAME                               )
               CALL SETMSG ( 'Unable to read summary record, #.' )
               CALL ERRINT ( '#', I                              )
               CALL SIGERR ( 'SPICE(DAFREADFAILURE)'             )
               CALL CHKOUT ( 'T_BGODAF'                          )
               RETURN
            END IF
 
            NXTDSC = INT(DPREC(1))
 
            CALL T_DAFWSR ( UNIT,
     .                      I,
     .                      OBFF,
     .                      ND,
     .                      NI,
     .                      INT(DPREC(1)),
     .                      INT(DPREC(2)),
     .                      INT(DPREC(3)),
     .                      DPREC(4)       )
 
C
C           Every summary record is followed by a name record...
C
            I = I + 1
 
            READ ( UNIT   = INUNIT,
     .             REC    = I,
     .             IOSTAT = IOSTAT ) CREC
 
            IF ( IOSTAT .NE. 0 ) THEN
               CALL DAFCLS ( INHAN                            )
               CALL KILFIL ( ONAME                            )
               CALL SETMSG ( 'Unable to read name record, #.' )
               CALL ERRINT ( '#', I                           )
               CALL SIGERR ( 'SPICE(DAFREADFAILURE)'          )
               CALL CHKOUT ( 'T_BGODAF'                       )
               RETURN
            END IF
 
            WRITE ( UNIT   = UNIT,
     .              REC    = I,
     .              IOSTAT = IOSTAT ) CREC
 
            IF ( IOSTAT .NE. 0 ) THEN
               CALL DAFCLS ( INHAN                             )
               CALL KILFIL ( ONAME                             )
               CALL SETMSG ( 'Unable to write name record, #.' )
               CALL ERRINT ( '#', I                            )
               CALL SIGERR ( 'SPICE(DAFWRITEFAILURE)'          )
               CALL CHKOUT ( 'T_BGODAF'                        )
               RETURN
            END IF
 
C
C        Otherwise we are dealing with a data record.
C
         ELSE
 
            CALL ZZDAFGDR ( INHAN, I, DPREC, FOUND )
 
            IF ( .NOT. FOUND ) THEN
               CALL DAFCLS ( INHAN                            )
               CALL KILFIL ( ONAME                            )
               CALL SETMSG ( 'Unable to read data record, #.' )
               CALL ERRINT ( '#', I                           )
               CALL SIGERR ( 'SPICE(DAFREADFAILURE)'          )
               CALL CHKOUT ( 'T_BGODAF'                       )
               RETURN
            END IF
 
            CALL T_DAFWDR ( UNIT, I, OBFF, 128, DPREC )
 
         END IF
 
C
C        Next record.
C
         I = I + 1
 
      END DO
 
C
C     Process the last data record, only translate up to (FREE-1)'s
C     address.
C
      CALL ZZDAFGDR ( INHAN, STPREC, DPREC, FOUND )
 
C
C     Do not worry if STPREC was not found, it is possible (but
C     very unlikely) that FREE points to a record that has yet
C     to be created.
C
      IF ( FOUND ) THEN
         CALL T_DAFWDR ( UNIT, I, OBFF, WORD-1, DPREC )
      END IF
 
C
C     Clean up.
C
      CALL DAFCLS ( INHAN )
 
C
C     Check FAILED() in case something has gone awry.  If it has delete
C     ONAME.
C
      IF ( FAILED() ) THEN
         CALL KILFIL ( ONAME )
      ELSE
         CLOSE ( UNIT = UNIT )
      END IF
 
      CALL CHKOUT ( 'T_BGODAF' )
      RETURN
 
      END
