C$Procedure T_BGODAS ( BINGO: Process DAS files to alternate BFFs )
 
      SUBROUTINE T_BGODAS ( INAME, ONAME, OBFF )      
 
C$ Abstract
C
C     Convert DAS files from one supported binary file format to
C     another.
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
C     INAME      is the name of the DAS file to convert to an alternate
C                binary file format.
C
C     ONAME      is the name of the converted DAS file to create. The
C                file named by ONAME, if extant at the time this
C                routine is called, will be deleted and replaced with
C                the converted DAS.
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
C$ Exceptions
C
C     1)  If an error occurs while reading records from INAME, the
C         error is diagnosed by a routine in the call tree of this
C         routine.
C
C     2)  If an error occurs while writing records to ONAME, the
C         error is diagnosed by a routine in the call tree of this
C         routine.
C
C     3)  Routines in the call tree of this routine may signal errors
C         as a result of improper inputs or other exceptional cases.
C
C$ Files
C
C     This routine opens the file named by INAME for read access and
C     creates the file named by ONAME.
C
C$ Particulars
C
C      This test routine allows existing test software that creates
C      DAS files using the high level writers to create native format
C      files and convert them to non-native format for testing
C      purposes.
C
C      As new binary file formats are added to the list of those
C      supported, this routine and routines it calls may require
C      updates.
C
C$ Examples
C
C     See F_EK01NN for an example of usage.
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
C     N.J. Bachman    (JPL)
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 11-FEB-2015 (NJB) (FST)
C
C
C-&

 
C
C     SPICELIB functions
C
      LOGICAL               EXISTS
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               DSCLOC
      PARAMETER           ( DSCLOC = 9 )

      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC = 2 )

      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )

      INTEGER               NWC
      PARAMETER           ( NWC    = 1024 )

      INTEGER               NWD
      PARAMETER           ( NWD    = 128 )

      INTEGER               NWI
      PARAMETER           ( NWI    = 256 )

      INTEGER               RNGLOC
      PARAMETER           ( RNGLOC = 3 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 4 )

C
C     DAS data type codes
C
      INTEGER               CHR
      PARAMETER           ( CHR    =  1  )

      INTEGER               DP
      PARAMETER           ( DP     =  2  )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )

C
C     Local variables
C
      CHARACTER*(NWC)       DATAC
      CHARACTER*(TYPLEN)    FTYPE
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME

      DOUBLE PRECISION      DATAD  ( NWD )

      INTEGER               CLTYPE
      INTEGER               CLSIZE
      INTEGER               DATAI  ( NWI )
      INTEGER               DIRLOC
      INTEGER               DIRREC ( NWI )
      INTEGER               FREE
      INTEGER               HIADDR ( 3 )
      INTEGER               I
      INTEGER               IHAN
      INTEGER               J
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               LOC
      INTEGER               NEXT   ( 3 )
      INTEGER               FWDPTR
      INTEGER               OHAN
      INTEGER               MXADDR ( 3 )
      INTEGER               NATBFF
      INTEGER               NCC
      INTEGER               NCR
      INTEGER               NRC
      INTEGER               NRR
      INTEGER               PREV   ( 3 )
      INTEGER               RECNO
      INTEGER               RECSIZ ( 3 )
      INTEGER               UNIT

      LOGICAL               PASS1

C
C     Saved variables
C     
      SAVE                  NATBFF
      SAVE                  NEXT
      SAVE                  PASS1
      SAVE                  PREV
      SAVE                  RECSIZ
      
C
C     Initial values
C
      DATA                  NEXT   / 2, 3, 1 /
      DATA                  PASS1  / .TRUE.  /
      DATA                  PREV   / 3, 1, 2 /
      DATA                  RECSIZ / NWC, NWD, NWI /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_BGODAS' )

      
      IF ( PASS1 ) THEN
C
C        Obtain the code for the native binary file format.
C        
         CALL ZZDDHNFC ( NATBFF )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_BGODAS' )
            RETURN
         END IF

         PASS1 = .FALSE.

      END IF

C
C     Open the existing input DAS file. Read the file's file record.
C
      CALL DASOPR ( INAME, IHAN )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_BGODAS' )
         RETURN
      END IF

C
C     Read the file record of the input DAS.
C
      CALL DASRFR ( IHAN, IDWORD, IFNAME, NRR, NRC, NCR, NCC )
      
      FTYPE = IDWORD(5:)

C
C     Get the file summary of the input DAS.
C
      CALL DASHFS ( IHAN, NRR,    NRC,    NCR,   NCC, 
     .              FREE, LASTLA, LASTRC, LASTWD      )

      IF ( EXISTS(ONAME) ) THEN
         CALL DELFIL ( ONAME )
      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_BGODAS' )
         RETURN
      END IF

      CALL DASONW ( ONAME, FTYPE, IFNAME, NCR, OHAN )


      CALL ZZDDHHLU ( OHAN, 'DAS', .TRUE., UNIT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_BGODAS' )
         RETURN
      END IF

C
C     Transfer the file record to the output file.
C
      IF ( OBFF .EQ. NATBFF ) THEN
C
C        The output format is native.
C        
         CALL DASWFR ( OHAN, IDWORD, IFNAME, NRR, 
     .                 NRC,  NCR,    NCC         )
      ELSE
C
C        This is a non-native write. We always add the FTP
C        string to the output file record.
C
         CALL T_DASWFR ( UNIT, OBFF, IDWORD, IFNAME, NRR,
     .                   NRC,  NCR,  NCC,    .TRUE.      )
      END IF

C
C     Transfer any reserved records and the comment area.
C
      DO I = 1, NRR

         RECNO = I + 1
         CALL DASRRC ( IHAN, RECNO, 1, NWC, DATAC )
         CALL DASWRC ( OHAN, RECNO,         DATAC )

      END DO

      DO I = 1, NCR

         RECNO = I + NRR + 1
         CALL DASRRC ( IHAN, RECNO, 1, NWC, DATAC )
         CALL DASWRC ( OHAN, RECNO,         DATAC )

      END DO

C
C     Transfer directories and the data records that follow them.
C
      DIRLOC = 2 + NRR + NCR

      DO WHILE ( DIRLOC .NE. 0 ) 
C
C        Read the current directory record; extract the forward pointer,
C        data address ranges, and first cluster descriptor.
C        
         CALL DASRRI ( IHAN, DIRLOC, 1, NWI, DIRREC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_BGODAS' )
            RETURN
         END IF

         FWDPTR = DIRREC(FWDLOC)

         DO I = 1, 3
C
C           HIADDR contains the highest address of each cluster that
C           has been processed, for each data type. MXADDR is the 
C           maximum address of each data type for the current 
C           directory.
C
C           Directories containing no clusters of a given data type
C           have address ranges of 0:0 for that type. Set HIADDR
C           accordingly.
C            
            J         = (2*I) - 1
            MXADDR(I) = DIRREC(RNGLOC+J  ) 

            IF ( MXADDR(I) .GT. 0 ) THEN

               HIADDR(I) = DIRREC(RNGLOC+J-1) - 1
            ELSE
               HIADDR(I) = 0
            END IF

         END DO

         CLTYPE = DIRREC(DSCLOC  )
         CLSIZE = DIRREC(DSCLOC+1)
         LOC    = DSCLOC + 1

C
C        Copy the directory record to the output file.
C
         IF ( OBFF .EQ. NATBFF ) THEN

            CALL DASWRI ( OHAN, DIRLOC, DIRREC )

         ELSE
C
C           This is the non-native output case.
C
            CALL T_XLTFWI ( DIRREC,  NWI,  OBFF,   DATAC )
            CALL DASIOC   ( 'WRITE', UNIT, DIRLOC, DATAC )

         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_BGODAS' )
            RETURN
         END IF

C
C        Traverse the directory record and copy the data records from
C        its associated clusters. The code below does not assume that
C        directory records are zero-padded.
C
         RECNO = DIRLOC + 1

         DO WHILE (      ( HIADDR(1) .LT. MXADDR(1) )
     .              .OR. ( HIADDR(2) .LT. MXADDR(2) )
     .              .OR. ( HIADDR(3) .LT. MXADDR(3) ) ) 

            DO I = 1, CLSIZE

               IF ( CLTYPE .EQ. CHR ) THEN
C
C                 Character records are written as is.
C
                  CALL DASRRC ( IHAN, RECNO, 1, NWC, DATAC )
                  CALL DASWRC ( OHAN, RECNO,         DATAC )

               ELSE IF ( CLTYPE .EQ. DP ) THEN

                  CALL DASRRD ( IHAN, RECNO, 1, NWD, DATAD )

                  IF ( OBFF .EQ. NATBFF ) THEN
C
C                    The output binary file format is native.
C                    Write the record as is.
C
                     CALL DASWRD ( OHAN, RECNO, DATAD )
 
                  ELSE
C
C                    Translate the d.p. record to the output
C                    format. The result is stored in a character
C                    string.
C                    
                     CALL T_XLTFWD ( DATAD,   NWD,  OBFF,  DATAC )
                     CALL DASIOC   ( 'WRITE', UNIT, RECNO, DATAC )

                  END IF

               ELSE IF ( CLTYPE .EQ. INT ) THEN

                  CALL DASRRI ( IHAN, RECNO, 1, NWI, DATAI )

                  IF ( OBFF .EQ. NATBFF ) THEN

                     CALL DASWRI ( OHAN, RECNO, DATAI )

                  ELSE

                     CALL T_XLTFWI ( DATAI,   NWI,  OBFF,  DATAC )
                     CALL DASIOC   ( 'WRITE', UNIT, RECNO, DATAC )

                  END IF

               ELSE
C
C                 We found a "new" data type code. The input file is 
C                 probably corrupted.
C
                  CALL SETMSG ( 'In DAS file #; directory record '
     .            //            'having record number # contains '
     .            //            'cluster type of #. The valid '
     .            //            'range is 1-3.'                  )
                  CALL ERRHAN ( '#', IHAN                        )
                  CALL ERRINT ( '#', DIRLOC                      )
                  CALL ERRINT ( '#', CLTYPE                      )
                  CALL SIGERR ( 'SPICE(DASBADDIRECTORY)'         )
                  CALL CHKOUT ( 'T_BGODAS'                       )
                  RETURN

               END IF
C
C              Process the next record of the cluster.
C
               RECNO = RECNO + 1

            END DO

C
C           Update the highest data address read for the data
C           type of the cluster that was just processed.
C
            HIADDR(CLTYPE) = HIADDR(CLTYPE)  +  CLSIZE * RECSIZ(CLTYPE)
C
C           Get the type and size of the next cluster.
C
            LOC = LOC + 1

            IF ( DIRREC(LOC) .LT. 0 ) THEN
               CLTYPE = PREV( CLTYPE )
            ELSE
               CLTYPE = NEXT( CLTYPE )
            END IF

            CLSIZE = ABS( DIRREC(LOC) )

         END DO

C
C        Set the directory record to the forward pointer.
C        Note that the indicated directory may be empty;
C        we still want to copy it to the output file.
C        
         DIRLOC = FWDPTR

      END DO

C     
C     Make sure all buffered data are written to the output file.
C
C     Note that for non-native output files, the only buffered
C     records will be of character type. These records don't
C     require translation, so DASWBR may be used to write them.
C 
C     An important side effect of this call is to deallocate 
C     buffer entries for this file in DASRWR. If these entries
C     remain allocated, later calls to DASRRC will probably fail,
C     as the buffered entries can't be written to make room for
C     new data.
C
      CALL DASWBR ( OHAN )

C
C     Close the output file without segregating it, since we're
C     duplicating the structure of the input file. If the input file is
C     segregated, the output file will be segregated as well,
C     automatically.
C
      CALL DASLLC ( OHAN )

C
C     We're done with the output DAS logical unit. Unlock it.
C
      CALL ZZDDHUNL ( OHAN, 'DAS' )

C
C     We're done with the input file.
C
      CALL DASCLS ( IHAN )

      CALL CHKOUT ( 'T_BGODAS' )
      RETURN
      END

