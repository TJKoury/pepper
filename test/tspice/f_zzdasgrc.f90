!$Procedure F_ZZDASGRC ( DAS record fetch tests )

      SUBROUTINE F_ZZDASGRC ( OK )

! $ Abstract
!
!     Exercise the private SPICELIB DAS record fetch routines.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'das.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the private SPICELIB DAS routines
!
!        ZZDASGRD
!        ZZDASGRI
!        ZZDASRFR
!
! $ Examples
!
!     None.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 31-MAY-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               EXISTS
      LOGICAL               FAILED

!
!     Local Parameters
!
      CHARACTER*(*)         DSK
      PARAMETER           ( DSK    = 'zzdasgrc_test.bds' )

      CHARACTER*(*)         NNDSK
      PARAMETER           ( NNDSK  = 'nn_zzdasgrc_test.bds' )


      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzdasgrc_test.tpc' )


!
!     These parameters must be kept consistent with zzddhman.inc.
!     That file is not included because of a parameter name conflict
!     with das.inc.
!
      INTEGER               BIGI3E
      PARAMETER           ( BIGI3E = 1 )

      INTEGER               LTLI3E
      PARAMETER           ( LTLI3E = 2 )

!
!     Other parameters
!
      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 40 )

      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               INTIX
      PARAMETER           ( INTIX  = 3 )

      INTEGER               DPIX
      PARAMETER           ( DPIX   = 2 )

!
!     Local Variables
!
      CHARACTER*(FRNMLN)    FRNAME
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(LBLSIZ)    LABEL
      CHARACTER*(IDWLEN)    XIDW
      CHARACTER*(IFNLEN)    XIFNAM

      DOUBLE PRECISION      DREC   ( NWD )
      DOUBLE PRECISION      XDREC  ( NWD )

      INTEGER               B
      INTEGER               BODYID
      INTEGER               CLBASE
      INTEGER               CLSIZE
      INTEGER               E
      INTEGER               FREE
      INTEGER               HANDLE
      INTEGER               NNHAN
      INTEGER               IREC   ( NWI )
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               NATBFF
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NREAD
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NW
      INTEGER               OBFF
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               SURFID
      INTEGER               UNIT
      INTEGER               WORDNO
      INTEGER               XIREC  ( NWI )
      INTEGER               XNCOMC
      INTEGER               XNCOMR
      INTEGER               XNRSVC
      INTEGER               XNRSVR

!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZDASGRC' )





!***********************************************************************
!
!     Setup
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create supporting kernels.' )

      IF ( EXISTS(PCK) ) THEN

         CALL DELFIL ( PCK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create tessellated ellipsoid DSK.' )

      BODYID = 499
      SURFID =   1
      FRNAME = 'IAU_MARS'

      NLON   = 40
      NLAT   = 20

      IF ( EXISTS(DSK) ) THEN

         CALL DELFIL ( DSK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


      CALL T_ELDS2Z ( BODYID, SURFID, FRNAME, NLON, NLAT, DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add a few comments to the DSK.
!
      CALL DASOPW ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASAC ( HANDLE, 1, 'Comment line number one out of one.' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create non-native version of DSK.' )

!
!     Get the native format code and the code of the format
!     to which the file will be transformed.
!
!
      CALL ZZDDHNFC ( NATBFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( NATBFF .EQ. LTLI3E ) THEN

         OBFF = BIGI3E

      ELSE IF ( NATBFF .EQ. BIGI3E ) THEN

         OBFF = LTLI3E

      ELSE

         CALL SETMSG ( 'Found unexpected native BFF code #.' )
         CALL ERRINT ( '#', NATBFF                           )
         CALL SIGERR ( 'SPICE(UNSUPPORTEDBFF)'               )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


      IF ( EXISTS(NNDSK) ) THEN

         CALL DELFIL ( NNDSK )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

      CALL T_BINGO ( DSK, NNDSK, OBFF )
      CALL CHCKXC ( .FALSE., ' ', OK )




!***********************************************************************
!
!     Test ZZDASRFR
!
!***********************************************************************

!***********************************************************************
!
!     Normal cases
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Open native DSK for read access; read file ' &
      //           'record.'                                    )


      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDASRFR ( HANDLE, IDWORD, IFNAME, NRESVR, &
                      NRESVC, NCOMR,  NCOMC          )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIDW   = 'DAS/DSK'
      XIFNAM = DSK
      XNRSVR = 0
      XNRSVC = 0
      XNCOMR = 1
      XNCOMC = 36

      CALL CHCKSC ( 'IDWORD', IDWORD, '=', XIDW,      OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', XIFNAM,    OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=', XNRSVR, 0, OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=', XNRSVC, 0, OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=', XNCOMR, 0, OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=', XNCOMC, 0, OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Open NON-native DSK for read access; read file ' &
      //           'record.'                                       )


      CALL DASOPR ( NNDSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZDASRFR ( HANDLE, IDWORD, IFNAME, NRESVR, &
                      NRESVC, NCOMR,  NCOMC          )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Use the expected values from the previous test case.
!
      CALL CHCKSC ( 'IDWORD', IDWORD, '=', XIDW,      OK )
      CALL CHCKSC ( 'IFNAME', IFNAME, '=', XIFNAM,    OK )
      CALL CHCKSI ( 'NRESVR', NRESVR, '=', XNRSVR, 0, OK )
      CALL CHCKSI ( 'NRESVC', NRESVC, '=', XNRSVC, 0, OK )
      CALL CHCKSI ( 'NCOMR',  NCOMR,  '=', XNCOMR, 0, OK )
      CALL CHCKSI ( 'NCOMC',  NCOMC,  '=', XNCOMC, 0, OK )

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
!     We test only exception (2) of ZZDASRFR. The others should
!     be covered by tests of lower-level routines.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDASRFR: Invalid file handle' )


      CALL ZZDASRFR ( -10000, IDWORD, IFNAME, NRESVR, &
                      NRESVC, NCOMR,  NCOMC          )
      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHHANDLE)', OK )


!
! --- Case: ------------------------------------------------------
!

!     CALL TCASE ( 'ZZDASRFR: error reading file' )
!
!     This test has the undesirable side effect of leaving behind
!     a file with a system-dependent file name. We exclude this
!     case from standard TSPICE runs, but it can be run independently.
!
!
!     Open the file for read access, then delete it.
!
!
!     CALL DASOPR ( DSK, HANDLE )
!     CALL CHCKXC ( .FALSE., ' ', OK )
!
!     CALL KILFIL ( DSK )
!
!     CALL ZZDASRFR ( HANDLE, IDWORD, IFNAME, NRESVR,
!    .                NRESVC, NCOMR,  NCOMC          )
!     CALL CHCKXC ( .TRUE., 'SPICE(FILEREADFAILED)', OK )
!




!***********************************************************************
!
!     Test ZZDASGRD
!
!***********************************************************************

!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check d.p. records read from native DSK.' )
!
!     This is a consistency check only: DASRDD uses ZZDASGRD.
!     The next test case uses the independent routine DASIOD.
!

!
!     Get the DSK's file summary info.
!
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )

!
!     Let NW be the number of d.p. data words in the file.
!
      NW = LASTLA( DPIX )

!
!     Read the d.p. data; compare with data that we fetch using
!     ZZDASGRD.
!

      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )

         NREAD = MIN( REMAIN, NWD )
         E     = B + NREAD - 1

         CALL DASRDD ( HANDLE, B, E, XDREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get the number of the record containing address B.
!
         CALL DASA2L ( HANDLE, DPIX,  B,     CLBASE, &
                       CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Read the record at index RECNO.
!
         CALL ZZDASGRD ( HANDLE, RECNO, DREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We expect an exact match.
!
         LABEL = 'DREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, DREC, '=', XDREC, NREAD, 0.D0, OK )

!
!        Prepare for the next read, if any.
!
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check d.p. records read from native DSK. ' &
      //           'Use unit-oriented record reader to fetch ' &
      //           'expected record.'                         )

!
!     Get the DSK's file summary info.
!
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )

!
!     Let NW be the number of d.p. data words in the file.
!
      NW = LASTLA( DPIX )

!
!     Read the d.p. data; compare with data that we fetch using
!     ZZDASGRD.
!

      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )

         NREAD = MIN( REMAIN, NWD )
         E     = B + NREAD - 1

!
!        Get the number of the record containing address B.
!
         CALL DASA2L ( HANDLE, DPIX,  B,     CLBASE, &
                       CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Get an unlocked logical unit for the DSK file.
!        Read the expected record.
!
         CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )

         CALL DASIOD ( 'READ', UNIT, RECNO, XDREC  )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Read the record at index RECNO.
!
         CALL ZZDASGRD ( HANDLE, RECNO, DREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We expect an exact match.
!
         LABEL = 'DREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, DREC, '=', XDREC, NREAD, 0.D0, OK )

!
!        Prepare for the next read, if any.
!
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check d.p. records read from non-native DSK.' )

!
!     Get the DSK's file summary info.
!
      CALL DASOPR ( NNDSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )

!
!     Let NW be the number of d.p. data words in the file.
!
      NW = LASTLA( DPIX )

!
!     Read the d.p. data; compare with data that we fetch using
!     ZZDASGRD.
!

      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )

         NREAD = MIN( REMAIN, NWD )
         E     = B + NREAD - 1

         CALL DASRDD ( HANDLE, B, E, XDREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get the number of the record containing address B.
!
         CALL DASA2L ( HANDLE, DPIX,  B,     CLBASE, &
                       CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Read the record at index RECNO.
!
         CALL ZZDASGRD ( HANDLE, RECNO, DREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We expect an exact match.
!
         LABEL = 'DREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, DREC, '=', XDREC, NREAD, 0.D0, OK )

!
!        Prepare for the next read, if any.
!
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check d.p. records read from non-native DSK, ' &
      //           'comparing directly to native data.'          )

!
!     Get the native DSK's file summary info.
!
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )


!
!     Open the non-native DSK for read access as well.
!
      CALL DASOPR ( NNDSK, NNHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Let NW be the number of d.p. data words in the file.
!
      NW = LASTLA( DPIX )

!
!     Read the d.p. data from the native file; compare with data that
!     we fetch from the non-native file using ZZDASGRD.
!

      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )

         NREAD = MIN( REMAIN, NWD )
         E     = B + NREAD - 1

         CALL DASRDD ( HANDLE, B, E, XDREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get the number of the record containing address B. We expect
!        this record number to be invariant across BFFs.
!
         CALL DASA2L ( HANDLE, DPIX,  B,     CLBASE, &
                       CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Read the record at index RECNO in the non-native file.
!
         CALL ZZDASGRD ( NNHAN, RECNO, DREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We expect an exact match.
!
         LABEL = 'DREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( LABEL, DREC, '=', XDREC, NREAD, 0.D0, OK )

!
!        Prepare for the next read, if any.
!
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DASCLS ( NNHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
!     We test only exception (1) of ZZDASGRD. The others should
!     be covered by tests of lower-level routines.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDASGRD: Invalid file handle' )


      CALL ZZDASGRD ( -10000, RECNO, DREC )
      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHHANDLE)', OK )


!***********************************************************************
!
!     Test ZZDASGRI
!
!***********************************************************************

!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check integer records read from native DSK.' )
!
!     This is a consistency check only: DASRDI uses ZZDASGRI.
!     The next test case uses the independent routine DASIOI.
!

!
!     Get the DSK's file summary info.
!
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )

!
!     Let NW be the number of integer data words in the file.
!
      NW = LASTLA( INTIX )

!
!     Read the integer data; compare with data that we fetch using
!     ZZDASGRI.
!

      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )

         NREAD = MIN( REMAIN, NWI )
         E     = B + NREAD - 1

         CALL DASRDI ( HANDLE, B, E, XIREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get the number of the record containing address B.
!
         CALL DASA2L ( HANDLE, INTIX, B,     CLBASE, &
                       CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Read the record at index RECNO.
!
         CALL ZZDASGRI ( HANDLE, RECNO, IREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We expect an exact match.
!
         LABEL = 'IREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, IREC, '=', XIREC, NREAD, OK )

!
!        Prepare for the next read, if any.
!
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check integer records read from native DSK.' &
      //           'Use unit-oriented record reader to fetch ' &
      //           'expected record.'                         )

!
!     Get the DSK's file summary info.
!
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )

!
!     Let NW be the number of integer data words in the file.
!
      NW = LASTLA( INTIX )

!
!     Read the integer data; compare with data that we fetch using
!     ZZDASGRI.
!

      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )

         NREAD = MIN( REMAIN, NWI )
         E     = B + NREAD - 1


!
!        Get the number of the record containing address B.
!
         CALL DASA2L ( HANDLE, INTIX, B,     CLBASE, &
                       CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get an unlocked unit and read the expected record.
!
         CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DASIOI ( 'READ', UNIT, RECNO, XIREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Read the record at index RECNO.
!
         CALL ZZDASGRI ( HANDLE, RECNO, IREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We expect an exact match.
!
         LABEL = 'IREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, IREC, '=', XIREC, NREAD, OK )

!
!        Prepare for the next read, if any.
!
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )






!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check integer records read from non-native DSK.' )

!
!     Get the DSK's file summary info.
!
      CALL DASOPR ( NNDSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )

!
!     Let NW be the number of integer data words in the file.
!
      NW = LASTLA( INTIX )

!
!     Read the integer data; compare with data that we fetch using
!     ZZDASGRI.
!

      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )

         NREAD = MIN( REMAIN, NWI )
         E     = B + NREAD - 1

         CALL DASRDI ( HANDLE, B, E, XIREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get the number of the record containing address B.
!
         CALL DASA2L ( HANDLE, INTIX, B,     CLBASE, &
                       CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Read the record at index RECNO.
!
         CALL ZZDASGRI ( HANDLE, RECNO, IREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We expect an exact match.
!
         LABEL = 'IREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, IREC, '=', XIREC, NREAD, OK )

!
!        Prepare for the next read, if any.
!
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check integer records read from non-native DSK, ' &
      //           'comparing directly to native data.'          )

!
!     Get the native DSK's file summary info.
!
      CALL DASOPR ( DSK, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DASHFS ( HANDLE, NRESVR, NRESVC, NCOMR, NCOMC, &
                    FREE,   LASTLA, LASTRC, LASTWD       )


!
!     Open the non-native DSK for read access as well.
!
      CALL DASOPR ( NNDSK, NNHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Let NW be the number integer data words in the file.
!
      NW = LASTLA( INTIX )

!
!     Read the integer data from the native file; compare with data that
!     we fetch from the non-native file using ZZDASGRI.
!

      REMAIN = NW
      B      = 1

      DO WHILE ( ( REMAIN .GT. 0 ) .AND. ( .NOT. FAILED() )  )

         NREAD = MIN( REMAIN, NWI )
         E     = B + NREAD - 1

         CALL DASRDI ( HANDLE, B, E, XIREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get the number of the record containing address B. We expect
!        this record number to be invariant across BFFs.
!
         CALL DASA2L ( HANDLE, INTIX, B,     CLBASE, &
                       CLSIZE, RECNO, WORDNO        )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Read the record at index RECNO in the non-native file.
!
         CALL ZZDASGRI ( NNHAN, RECNO, IREC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We expect an exact match.
!
         LABEL = 'IREC@ record #'
         CALL REPMI ( LABEL, '#', RECNO, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAI ( LABEL, IREC, '=', XIREC, NREAD, OK )

!
!        Prepare for the next read, if any.
!
         REMAIN = REMAIN - NREAD
         B      = B      + NREAD

      END DO

      CALL DASCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DASCLS ( NNHAN )
      CALL CHCKXC ( .FALSE., ' ', OK )



!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
!     We test only exception (1) of ZZDASGRI. The others should
!     be covered by tests of lower-level routines.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDASGRI: Invalid file handle' )


      CALL ZZDASGRI ( -10000, RECNO, IREC )
      CALL CHCKXC ( .TRUE., 'SPICE(NOSUCHHANDLE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up: delete kernels.' )

      IF ( EXISTS(PCK) ) THEN
         CALL DELFIL ( PCK )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL UNLOAD ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( NNDSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( NNDSK )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


