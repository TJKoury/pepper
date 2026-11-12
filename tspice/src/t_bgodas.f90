!$Procedure T_BGODAS ( BINGO: Process DAS files to alternate BFFs )

      SUBROUTINE T_BGODAS ( INAME, ONAME, OBFF )
      use pepper_legacy

! $ Abstract
!
!     Convert DAS files from one supported binary file format to
!     another.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST ROUTINE
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         INAME
      CHARACTER*(*)         ONAME
      INTEGER               OBFF

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INAME      I   Name of source file to convert.
!     ONAME      I   Name of output file to create.
!     OBFF       I   Integer code for binary file format of ONAME.
!
! $ Detailed_Input
!
!     INAME      is the name of the DAS file to convert to an alternate
!                binary file format.
!
!     ONAME      is the name of the converted DAS file to create. The
!                file named by ONAME, if extant at the time this
!                routine is called, will be deleted and replaced with
!                the converted DAS.
!
!     OBFF       is an integer code that indicates the binary file
!                format targeted for ONAME.  Acceptable values
!                are the parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!                as defined in the include file 'zzddhman.inc'.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while reading records from INAME, the
!         error is diagnosed by a routine in the call tree of this
!         routine.
!
!     2)  If an error occurs while writing records to ONAME, the
!         error is diagnosed by a routine in the call tree of this
!         routine.
!
!     3)  Routines in the call tree of this routine may signal errors
!         as a result of improper inputs or other exceptional cases.
!
! $ Files
!
!     This routine opens the file named by INAME for read access and
!     creates the file named by ONAME.
!
! $ Particulars
!
!      This test routine allows existing test software that creates
!      DAS files using the high level writers to create native format
!      files and convert them to non-native format for testing
!      purposes.
!
!      As new binary file formats are added to the list of those
!      supported, this routine and routines it calls may require
!      updates.
!
! $ Examples
!
!     See F_EK01NN for an example of usage.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 11-FEB-2015 (NJB) (FST)
!
!
!-&


!
!     SPICELIB functions
!

!
!     Local parameters
!
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

!
!     DAS data type codes
!
      INTEGER               CHR
      PARAMETER           ( CHR    =  1  )

      INTEGER               DP
      PARAMETER           ( DP     =  2  )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )

!
!     Local variables
!
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

!
!     Saved variables
!
      SAVE                  NATBFF
      SAVE                  NEXT
      SAVE                  PASS1
      SAVE                  PREV
      SAVE                  RECSIZ

!
!     Initial values
!
      DATA                  NEXT   / 2, 3, 1 /
      DATA                  PASS1  / .TRUE.  /
      DATA                  PREV   / 3, 1, 2 /
      DATA                  RECSIZ / NWC, NWD, NWI /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_BGODAS' )


      IF ( PASS1 ) THEN
!
!        Obtain the code for the native binary file format.
!
         CALL ZZDDHNFC ( NATBFF )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_BGODAS' )
            RETURN
         END IF

         PASS1 = .FALSE.

      END IF

!
!     Open the existing input DAS file. Read the file's file record.
!
      CALL DASOPR ( INAME, IHAN )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_BGODAS' )
         RETURN
      END IF

!
!     Read the file record of the input DAS.
!
      CALL DASRFR ( IHAN, IDWORD, IFNAME, NRR, NRC, NCR, NCC )

      FTYPE = IDWORD(5:)

!
!     Get the file summary of the input DAS.
!
      CALL DASHFS ( IHAN, NRR,    NRC,    NCR,   NCC, &
                    FREE, LASTLA, LASTRC, LASTWD      )

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

!
!     Transfer the file record to the output file.
!
      IF ( OBFF .EQ. NATBFF ) THEN
!
!        The output format is native.
!
         CALL DASWFR ( OHAN, IDWORD, IFNAME, NRR, &
                       NRC,  NCR,    NCC         )
      ELSE
!
!        This is a non-native write. We always add the FTP
!        string to the output file record.
!
         CALL T_DASWFR ( UNIT, OBFF, IDWORD, IFNAME, NRR, &
                         NRC,  NCR,  NCC,    .TRUE.      )
      END IF

!
!     Transfer any reserved records and the comment area.
!
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

!
!     Transfer directories and the data records that follow them.
!
      DIRLOC = 2 + NRR + NCR

      DO WHILE ( DIRLOC .NE. 0 )
!
!        Read the current directory record; extract the forward pointer,
!        data address ranges, and first cluster descriptor.
!
         CALL DASRRI ( IHAN, DIRLOC, 1, NWI, DIRREC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_BGODAS' )
            RETURN
         END IF

         FWDPTR = DIRREC(FWDLOC)

         DO I = 1, 3
!
!           HIADDR contains the highest address of each cluster that
!           has been processed, for each data type. MXADDR is the
!           maximum address of each data type for the current
!           directory.
!
!           Directories containing no clusters of a given data type
!           have address ranges of 0:0 for that type. Set HIADDR
!           accordingly.
!
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

!
!        Copy the directory record to the output file.
!
         IF ( OBFF .EQ. NATBFF ) THEN

            CALL DASWRI ( OHAN, DIRLOC, DIRREC )

         ELSE
!
!           This is the non-native output case.
!
            CALL T_XLTFWI ( DIRREC,  NWI,  OBFF,   DATAC )
            CALL DASIOC   ( 'WRITE', UNIT, DIRLOC, DATAC )

         END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_BGODAS' )
            RETURN
         END IF

!
!        Traverse the directory record and copy the data records from
!        its associated clusters. The code below does not assume that
!        directory records are zero-padded.
!
         RECNO = DIRLOC + 1

         DO WHILE (      ( HIADDR(1) .LT. MXADDR(1) ) &
                    .OR. ( HIADDR(2) .LT. MXADDR(2) ) &
                    .OR. ( HIADDR(3) .LT. MXADDR(3) ) )

            DO I = 1, CLSIZE

               IF ( CLTYPE .EQ. CHR ) THEN
!
!                 Character records are written as is.
!
                  CALL DASRRC ( IHAN, RECNO, 1, NWC, DATAC )
                  CALL DASWRC ( OHAN, RECNO,         DATAC )

               ELSE IF ( CLTYPE .EQ. DP ) THEN

                  CALL DASRRD ( IHAN, RECNO, 1, NWD, DATAD )

                  IF ( OBFF .EQ. NATBFF ) THEN
!
!                    The output binary file format is native.
!                    Write the record as is.
!
                     CALL DASWRD ( OHAN, RECNO, DATAD )

                  ELSE
!
!                    Translate the d.p. record to the output
!                    format. The result is stored in a character
!                    string.
!
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
!
!                 We found a "new" data type code. The input file is
!                 probably corrupted.
!
                  CALL SETMSG ( 'In DAS file #; directory record ' &
                  //            'having record number # contains ' &
                  //            'cluster type of #. The valid ' &
                  //            'range is 1-3.'                  )
                  CALL ERRHAN ( '#', IHAN                        )
                  CALL ERRINT ( '#', DIRLOC                      )
                  CALL ERRINT ( '#', CLTYPE                      )
                  CALL SIGERR ( 'SPICE(DASBADDIRECTORY)'         )
                  CALL CHKOUT ( 'T_BGODAS'                       )
                  RETURN

               END IF
!
!              Process the next record of the cluster.
!
               RECNO = RECNO + 1

            END DO

!
!           Update the highest data address read for the data
!           type of the cluster that was just processed.
!
            HIADDR(CLTYPE) = HIADDR(CLTYPE)  +  CLSIZE * RECSIZ(CLTYPE)
!
!           Get the type and size of the next cluster.
!
            LOC = LOC + 1

            IF ( DIRREC(LOC) .LT. 0 ) THEN
               CLTYPE = PREV( CLTYPE )
            ELSE
               CLTYPE = NEXT( CLTYPE )
            END IF

            CLSIZE = ABS( DIRREC(LOC) )

         END DO

!
!        Set the directory record to the forward pointer.
!        Note that the indicated directory may be empty;
!        we still want to copy it to the output file.
!
         DIRLOC = FWDPTR

      END DO

!
!     Make sure all buffered data are written to the output file.
!
!     Note that for non-native output files, the only buffered
!     records will be of character type. These records don't
!     require translation, so DASWBR may be used to write them.
!
!     An important side effect of this call is to deallocate
!     buffer entries for this file in DASRWR. If these entries
!     remain allocated, later calls to DASRRC will probably fail,
!     as the buffered entries can't be written to make room for
!     new data.
!
      CALL DASWBR ( OHAN )

!
!     Close the output file without segregating it, since we're
!     duplicating the structure of the input file. If the input file is
!     segregated, the output file will be segregated as well,
!     automatically.
!
      CALL DASLLC ( OHAN )

!
!     We're done with the output DAS logical unit. Unlock it.
!
      CALL ZZDDHUNL ( OHAN, 'DAS' )

!
!     We're done with the input file.
!
      CALL DASCLS ( IHAN )

      CALL CHKOUT ( 'T_BGODAS' )
      RETURN
      END

