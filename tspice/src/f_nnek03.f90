!$Procedure F_NNEK03 ( EK tests, subset 3 )

      SUBROUTINE F_NNEK03 ( OK )
      use pepper_legacy
      use pepper_inc_ekattdsc
      use pepper_inc_ekbool
      use pepper_inc_ekcnamsz
      use pepper_inc_ekglimit
      use pepper_inc_ekqlimit
      use pepper_inc_ektnamsz
      use pepper_inc_ektype
      use pepper_inc_errhnd
      use pepper_inc_zzddhman

! $ Abstract
!
!     Exercise the SPICELIB EK routines, subset 3. This routine
!     operates on EK files having non-native binary format.
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
!     This routine tests the following subset of the EK routines:
!
!        EKCCNT
!        EKCII
!        EKNTAB
!        EKTNAM
!
!     This routine operates on EK files having non-native binary
!     format.
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
!-    TSPICE Version 1.0.0, 12-FEB-2015 (NJB)
!
!        Based on F_EK03 TSPICE Version 1.0.1, 20-FEB-2009 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      CHARACTER*(*)         EK1
      PARAMETER           ( EK1    = 'test1nn.ek' )

      CHARACTER*(*)         EK1NAT
      PARAMETER           ( EK1NAT = 'test1.ek' )

      CHARACTER*(*)         EK2
      PARAMETER           ( EK2    = 'test2nn.ek' )

      CHARACTER*(*)         EK2NAT
      PARAMETER           ( EK2NAT = 'test2.ek' )

      INTEGER               DECLEN
      PARAMETER           ( DECLEN = 200 )

      INTEGER               MAXROW
      PARAMETER           ( MAXROW = 20 )


!
!     Local Variables
!
      CHARACTER*(CNAMSZ)    CNAMES ( MXCLSG )
      CHARACTER*(CNAMSZ)    COLNAM
      CHARACTER*(DECLEN)    DECLS  ( MXCLSG )
      CHARACTER*(LMSGLN)    MSG
      CHARACTER*(TNAMSZ)    TABNAM

      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               DIMS   ( MXCLSG )
      INTEGER               DTYPES ( MXCLSG )
      INTEGER               CCLASS ( MXCLSG )
      INTEGER               FILENO
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               NATBFF
      INTEGER               NCOLS
      INTEGER               NNBFF
      INTEGER               NROWS
      INTEGER               NSEG
      INTEGER               NTAB
      INTEGER               SEGTYP
      INTEGER               STLENS ( MXCLSG )
      INTEGER               TAB
      INTEGER               XNCOLS

      LOGICAL               BOOL
      LOGICAL               INDEXD ( MXCLSG )
      LOGICAL               NULLOK ( MXCLSG )


!
!     Open the test family.
!
      CALL TOPEN ( 'F_NNEK03' )


!
! --- Case: ------------------------------------------------------
!
!
! --- Setup
!
      CALL TCASE ( 'Set native and non-native binary file format ' &
      //           'codes.'                                          )


      CALL ZZDDHNFC ( NATBFF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( NATBFF .EQ. LTLI3E ) THEN

         NNBFF = BIGI3E
      ELSE
         NNBFF = LTLI3E
      END IF

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Setup:  create an EK.' )
!
!     Create an EK.
!
      FILENO = 1

      CALL TSTEK  ( EK1NAT, FILENO, MAXROW, .FALSE., HANDLE )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create non-native EK from native version.' )

!
!     Replace the EK with a non-native version.
!
      CALL T_BINGO ( EK1NAT, EK1, NNBFF )
      CALL CHCKXC ( .FALSE., ' ', OK    )

      CALL DELFIL ( EK1NAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Register the non-native EK with the FILREG so that it can be
!     removed when the current test family is done with its
!     task.
!
      CALL TFILES ( EK1 )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test EKNTAB: make sure the number of tables '// &
                   'matches the number of segments in EK1.'        )
!
!     Find out how many segments are in the EK.  By the specification
!     of TSTEK, there's one segment per table.
!
      CALL EKOPR  ( EK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      NSEG = EKNSEG ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close the EK.
!
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
!     Load the EK into the query system.
!
      CALL FURNSH ( EK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Continuing previous case; creating EK2' )

!
!     Make a copy of EK1 and load it as well.
!
      CALL TSTEK  ( EK2NAT, FILENO, MAXROW, .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Replace the EK with a non-native version.
!
      CALL T_BINGO ( EK2NAT, EK2, NNBFF )
      CALL CHCKXC ( .FALSE., ' ', OK    )

      CALL DELFIL ( EK2NAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Register the non-native EK with the FILREG so that it can be
!     removed when the current test family is done with its
!     task.
!
      CALL TFILES ( EK2 )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Continuing previous case; using EK2' )

      CALL FURNSH ( EK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the number of loaded tables.
!
      CALL EKNTAB ( NTAB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'Number of loaded tables NTAB', &
                    NTAB, '=', NSEG, 0, OK           )


!
!     Here ntab is the number of tables looked up via EKNTAB.
!
      DO TAB = 1, NTAB
!
!        Start a new test case.
!
         MSG = 'Testing EKTNAM, EKCCNT, EKCII for table #'
         CALL REPMI ( MSG, '#', TAB, MSG )

         CALL TCASE ( MSG )

!
!        Get the table name.
!
         CALL EKTNAM ( TAB, TABNAM )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Look up the schema for this table.
!
         CALL TSTSCH ( TABNAM, MAXROW, SEGTYP, NROWS,  XNCOLS, &
                       CNAMES, CCLASS, DTYPES, STLENS, DIMS, &
                       INDEXD, NULLOK, DECLS                   )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Get the column count.
!
         CALL EKCCNT ( TABNAM, NCOLS )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'NCOLS from EKCCNT', &
                        NCOLS, '=', XNCOLS, 0, OK )

!
!        For each column in the current table, look up the column's
!        attributes.  The attribute block index parameters are defined
!        in the include file ekattdsc.inc.
!
         DO I = 1, NCOLS
!
!           Look up the attribute information for the ith column.
!
            CALL EKCII ( TABNAM, I, COLNAM, ATTDSC )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Check the column name.
!
            CALL CHCKSC ( 'COLNAM', COLNAM, '=', CNAMES(I), OK )

!
!           Check the current column's class.
!
            CALL CHCKSI ( 'COLNAM class', &
                           ATTDSC(ATTCLS), '=', CCLASS(I), 0, OK )

!
!           Check the current column's data type.
!
            CALL CHCKSI ( 'COLNAM data type', &
                           ATTDSC(ATTTYP), '=', DTYPES(I), 0, OK )

!
!           If the data type is character, check the string length.
!
            IF ( ATTDSC(ATTTYP) .EQ. CHR ) THEN

                CALL CHCKSI ( 'COLNAM string length', &
                              ATTDSC(ATTLEN), '=', STLENS(I), 0, OK )
            END IF

!
!           Check the current column's entry size.
!
            CALL CHCKSI ( 'COLNAM entry size', &
                           ATTDSC(ATTSIZ), '=', DIMS(I), 0, OK )


!
!           Check the current column's index flag.
!
            BOOL = ATTDSC(ATTIDX) .EQ. ITRUE

            CALL CHCKSL ( 'COLNAM index flag', BOOL, INDEXD(I), OK )

!
!           Check the current column's null ok flag.
!
            BOOL = ATTDSC(ATTNFL) .EQ. ITRUE

            CALL CHCKSL ( 'COLNAM null ok flag', BOOL, NULLOK(I), OK )

         END DO

!
!        We're done with the current column.
!

      END DO
!
!     We're done with the current table.
!


!
!     Unload the EKs.  The TSPICE system will delete the files.
!
      CALL TCASE  ( 'Unload EKs from query system.' )

      CALL UNLOAD ( EK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( EK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


