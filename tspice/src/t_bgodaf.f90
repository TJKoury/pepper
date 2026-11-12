!$Procedure T_BGODAF ( BINGO: Process DAF files to alternate BFFs )

      SUBROUTINE T_BGODAF ( INAME, ONAME, OBFF )
      use pepper_legacy

! $ Abstract
!
!     Convert DAFs from one supported binary file format to another.
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
!     INAME      is the name of the DAF file to convert to an
!                alternate binary file format.
!
!     ONAME      is the name of the converted DAF file to create. The
!                file named by ONAME will be destroyed and replaced
!                with the converted DAF.
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
! $ Files
!
!     This routine opens the file named by INAME for read access and
!     creates the file named by ONAME.
!
! $ Exceptions
!
!     1) SPICE(DAFREADFAILURE) is signaled whenever reading records
!        from INAME fails.  In the event such an error occurs,
!        ONAME is deleted.
!
!     2) SPICE(DAFWRITEFAILURE) is signaled whenever writing records
!        to ONAME fails.  In the event such an error occurs, ONAME
!        is deleted.
!
!     3) Routines in the call tree of this routine may signal errors
!        as a result of improper inputs or other exceptional cases.
!
! $ Particulars
!
!      This test routine allows existing test software that creates
!      DAFs using the high level writers to create native format
!      files and convert them to non-native format for testing
!      purposes.
!
!      As new binary file formats are added to the list of those
!      supported, this routine and routines it calls may require
!      updates.
!
! $ Examples
!
!     See some TBD test routine for usage.
!
! $ Restrictions
!
!     1) ONAME must reference a file currently not connected to
!        a unit.  Unpredictable behavior may result otherwise.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 28-NOV-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!

!
!     Local Variables
!
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

!
!     Standard SPICE error handling
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_BGODAF' )
      END IF

!
!     Open INAME for read access.
!
      CALL DAFOPR ( INAME, INHAN )

!
!     Create ONAME.
!
      CALL T_DAFOPN ( ONAME, OBFF, UNIT )

!
!     Read INAME's file record, we need everything so use the very low
!     level reader.
!
      CALL ZZDAFGFR ( INHAN, IDWORD, ND, NI, IFNAME, FWARD, BWARD, &
                      FREE,  FOUND                                 )

      IF ( .NOT. FOUND ) THEN
         CALL DAFCLS ( INHAN                         )
         CALL KILFIL ( ONAME                         )
         CALL SETMSG ( 'Unable to find file record.' )
         CALL SIGERR ( 'SPICE(DAFREADFAILURE)'       )
         CALL CHKOUT ( 'T_BGODAF'                    )
         RETURN
      END IF

!
!     Dump the file record into ONAME.  We add the FTP string
!     regardless... all new toolkits create files with the FTP
!     string.
!
      CALL T_DAFWFR ( UNIT, OBFF, IDWORD, ND, NI, IFNAME, FWARD, &
                      BWARD, FREE, .TRUE. )

!
!     Now just copy all the records between the file record and
!     FWARD.  This is valid at the moment for all supported
!     platforms, but may need to change in the future.
!
      CALL ZZDDHHLU ( INHAN, 'DAF', .FALSE., INUNIT )

      DO I = 2, (FWARD-1)

         READ ( UNIT   = INUNIT, &
                REC    = I, &
                IOSTAT = IOSTAT ) CREC

         IF ( IOSTAT .NE. 0 ) THEN
            CALL DAFCLS ( INHAN                               )
            CALL KILFIL ( ONAME                               )
            CALL SETMSG ( 'Unable to read comment record, #.' )
            CALL ERRINT ( '#', I                              )
            CALL SIGERR ( 'SPICE(DAFREADFAILURE)'             )
            CALL CHKOUT ( 'T_BGODAF'                          )
            RETURN
         END IF

         WRITE ( UNIT   = UNIT, &
                 REC    = I, &
                 IOSTAT = IOSTAT ) CREC

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

!
!     Now process the summary, name, and data records.  The record
!     FREE points to is the last record to be processed.
!
      CALL DAFARW ( FREE, STPREC, WORD )

      I      = FWARD
      NXTDSC = FWARD

      DO WHILE ( I .LT. STPREC )

!
!        See if the record we are currently processing is a
!        descriptor record.
!
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

            CALL T_DAFWSR ( UNIT, &
                            I, &
                            OBFF, &
                            ND, &
                            NI, &
                            INT(DPREC(1)), &
                            INT(DPREC(2)), &
                            INT(DPREC(3)), &
                            DPREC(4)       )

!
!           Every summary record is followed by a name record...
!
            I = I + 1

            READ ( UNIT   = INUNIT, &
                   REC    = I, &
                   IOSTAT = IOSTAT ) CREC

            IF ( IOSTAT .NE. 0 ) THEN
               CALL DAFCLS ( INHAN                            )
               CALL KILFIL ( ONAME                            )
               CALL SETMSG ( 'Unable to read name record, #.' )
               CALL ERRINT ( '#', I                           )
               CALL SIGERR ( 'SPICE(DAFREADFAILURE)'          )
               CALL CHKOUT ( 'T_BGODAF'                       )
               RETURN
            END IF

            WRITE ( UNIT   = UNIT, &
                    REC    = I, &
                    IOSTAT = IOSTAT ) CREC

            IF ( IOSTAT .NE. 0 ) THEN
               CALL DAFCLS ( INHAN                             )
               CALL KILFIL ( ONAME                             )
               CALL SETMSG ( 'Unable to write name record, #.' )
               CALL ERRINT ( '#', I                            )
               CALL SIGERR ( 'SPICE(DAFWRITEFAILURE)'          )
               CALL CHKOUT ( 'T_BGODAF'                        )
               RETURN
            END IF

!
!        Otherwise we are dealing with a data record.
!
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

!
!        Next record.
!
         I = I + 1

      END DO

!
!     Process the last data record, only translate up to (FREE-1)'s
!     address.
!
      CALL ZZDAFGDR ( INHAN, STPREC, DPREC, FOUND )

!
!     Do not worry if STPREC was not found, it is possible (but
!     very unlikely) that FREE points to a record that has yet
!     to be created.
!
      IF ( FOUND ) THEN
         CALL T_DAFWDR ( UNIT, I, OBFF, WORD-1, DPREC )
      END IF

!
!     Clean up.
!
      CALL DAFCLS ( INHAN )

!
!     Check FAILED() in case something has gone awry.  If it has delete
!     ONAME.
!
      IF ( FAILED() ) THEN
         CALL KILFIL ( ONAME )
      ELSE
         CLOSE ( UNIT = UNIT )
      END IF

      CALL CHKOUT ( 'T_BGODAF' )
      RETURN

      END
