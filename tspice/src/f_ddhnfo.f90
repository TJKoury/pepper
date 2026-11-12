!$Procedure F_DDHNFO ( ZZDDHNFO Test Family )

      SUBROUTINE F_DDHNFO ( OK )
      use pepper_legacy
      use pepper_inc_zzddhman

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDDHNFO
!     routine.
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
! $ Files
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
! $ Particulars
!
!     This routine exercises ZZDDHNFO's logic.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
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
!-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
!
!        Updated module as the result of changes to the handle
!        manager interface, ZZDDHCLS.
!
!-    TSPICE Version 1.0.0, 05-SEP-2001 (FST)
!
!-&

!
!     Local Parameters
!
!     NUMFIL is the maximum number of different possible types of files
!     that this system may open in the ZZDDHMAN interface.  It is
!     computed based on the following:
!
!        There are NUMARC architectures.  Each method is valid for
!        either architecture.
!
!        All non-native files may only be opened for 'READ' access.
!
      INTEGER               NUMFIL
      PARAMETER           ( NUMFIL = NUMARC*(NUMAMH+NUMBFF) )

!
!     Test Table
!
      CHARACTER*(FILEN)     TBLFNM ( NUMFIL )
      INTEGER               TBLAMH ( NUMFIL )
      INTEGER               TBLARC ( NUMFIL )
      INTEGER               TBLBFF ( NUMFIL )
      INTEGER               TBLHAN ( NUMFIL )

!
!     Local Variables
!
      CHARACTER*(FILEN)     FNAME
      CHARACTER*(FILEN)     FNMPAT
      CHARACTER*(STRSIZ)    STRAMH ( NUMAMH )
      CHARACTER*(STRSIZ)    STRARC ( NUMARC )
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(80)        TCSTR
      CHARACTER*(FILEN)     TSTSTR

      INTEGER               FILCNT
      INTEGER               I
      INTEGER               INTAMH
      INTEGER               INTARC
      INTEGER               INTBFF
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               K
      INTEGER               NATBFF
      INTEGER               NUMSUP
      INTEGER               SUPBFF ( NUMARC )
      INTEGER               UNIT

      LOGICAL               FOUND

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_DDHNFO' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'F_DDHNFO Initialization' )

!
!     Set the filename pattern we will use.
!
      FNMPAT = 'test#.fil'

!
!     Fetch some initialization data.
!
      CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, STRAMH, STRARC, STRBFF )

!
!     Now create entries in the test table for each of the native
!     binary file format files of each architecture and access method.
!
      DO I = 1, ( NUMARC * NUMAMH )

         CALL REPMI ( FNMPAT, '#', I, TBLFNM(I) )
         TBLARC(I) = MOD(I, NUMARC) + 1
         TBLAMH(I) = (I-1)/NUMARC+1
         TBLBFF(I) = NATBFF

      END DO

!
!     Create entries in the test table for each of the non-native
!     binary file format files of each architecture with 'READ' access.
!
      I = ( NUMARC * NUMAMH )

      DO J = 1, NUMSUP

!
!        We are going to ignore the native binary file format, since
!        we addressed it in the preceding loop.
!
         IF ( SUPBFF(J) .NE. NATBFF ) THEN

!
!           Loop over every possible architecture, preparing the
!           list of files for 'READ' access.
!
            DO K = 1, NUMARC

               I = I + 1

               CALL REPMI ( FNMPAT, '#', I, TBLFNM(I) )
               TBLARC(I) = MOD(I, NUMARC) + 1
               TBLAMH(I) = READ
               TBLBFF(I) = SUPBFF(J)

            END DO

         END IF

      END DO

!
!     Store the number of files we placed into the handle list.
!
      FILCNT = I

!
!     Now create the files.
!
      DO I = 1, FILCNT

!
!        Only create files for access methods that are not 'NEW'
!        or 'SCRATCH'.
!
         IF (       ( TBLAMH(I) .NE. NEW    ) &
              .AND. ( TBLAMH(I) .NE. SCRTCH )  ) THEN

!
!           Create the file.
!
            CALL T_CPTFIL ( TBLFNM(I), &
                            TBLARC(I), &
                            2, &
                            STRBFF(TBLBFF(I)), &
                            'ABCD', &
                            'EFGH', &
                            'IJKL', &
                            .TRUE., &
                            .FALSE., &
                            ' '      )

         END IF

!
!        Open the file into the handle manager.
!
         CALL ZZDDHOPN ( TBLFNM(I), &
                         STRAMH(TBLAMH(I)), &
                         STRARC(TBLARC(I)), &
                         TBLHAN(I)          )

      END DO

!
!     Now that we have finished all of the initialization stuff,
!     Perform the checks on each of the files in the test table.
!
      DO I = 1, FILCNT

!
!        Treat each file as an individual case.  Generate the
!        TCASE string automatically from the data in the table.
!
         TCSTR = 'Exercise - # # # Nominal Logic'

         CALL REPMC ( TCSTR, '#', STRAMH(TBLAMH(I)), TCSTR )
         CALL REPMC ( TCSTR, '#', STRBFF(TBLBFF(I)), TCSTR )
         CALL REPMC ( TCSTR, '#', STRARC(TBLARC(I)), TCSTR )

         CALL TCASE ( TCSTR )

!
!        Now setup the inputs and outputs.
!
         FNAME  = ' '
         INTARC = 0
         INTBFF = 0
         INTAMH = 0
         FOUND  = .FALSE.

!
!        Invoke the module.
!
         CALL ZZDDHNFO ( TBLHAN(I), &
                         FNAME, &
                         INTARC, &
                         INTBFF, &
                         INTAMH, &
                         FOUND      )

!
!        Check for the absence of an exception.
!
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check outputs.
!
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Handle the case of scratch filenames separately.
!        Since their names are constructed automatically.
!
         IF ( TBLAMH(I) .EQ. SCRTCH ) THEN

!
!           INQUIRE on the UNIT associated with the scratch
!           handle.
!
            CALL ZZDDHHLU ( TBLHAN(I), STRARC(TBLARC(I)), &
                            .FALSE.,   UNIT               )

!
!           Set the default value of the filename, in case the
!           INQUIRE does not change it.
!
            TSTSTR = '# SCRATCH FILE'
            CALL REPMC ( TSTSTR, '#', STRARC(TBLARC(I)), TSTSTR )

            INQUIRE ( UNIT = UNIT, &
                      NAME = TSTSTR, &
                      IOSTAT = IOSTAT )

!
!           Check the IOSTAT from the INQUIRE.
!
            CALL CHCKSI ( 'IOSTAT', IOSTAT, '=', 0, 0, OK )

         ELSE

            TSTSTR = TBLFNM(I)

         END IF

         CALL CHCKSC ( 'FNAME',  FNAME,  '=', TSTSTR,    OK )

         CALL CHCKSI ( 'INTARC', INTARC, '=', TBLARC(I), 0, OK )
         CALL CHCKSI ( 'INTBFF', INTBFF, '=', TBLBFF(I), 0, OK )
         CALL CHCKSI ( 'INTAMH', INTAMH, '=', TBLAMH(I), 0, OK )

      END DO

!
!     Check for the absence of a rogue exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise Handle Not Found Exception Logic' )

!
!     Setup inputs and outputs.
!
      FNAME  = 'BOGUS FILENAME'
      INTARC = 1
      INTBFF = 1
      INTAMH = 1
      FOUND  = .TRUE.

!
!     Invoke the module on the zero-valued handle.
!
      CALL ZZDDHNFO ( 0, FNAME, INTARC, INTBFF, INTAMH, FOUND )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL CHCKSC ( 'FNAME', FNAME, '=', ' ', OK )

      CALL CHCKSI ( 'INTARC', INTARC, '=', 0, 0, OK )
      CALL CHCKSI ( 'INTBFF', INTBFF, '=', 0, 0, OK )
      CALL CHCKSI ( 'INTAMH', INTAMH, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Exercise Handle Opposite Sign Exception Logic' )

!
!     Setup inputs and outputs.
!
      FNAME  = 'BOGUS FILENAME'
      INTARC = 1
      INTBFF = 1
      INTAMH = 1
      FOUND  = .TRUE.

!
!     Invoke the module on the negative of an existing, in-use handle.
!
         CALL ZZDDHNFO ( -TBLHAN(1), &
                         FNAME, &
                         INTARC, &
                         INTBFF, &
                         INTAMH, &
                         FOUND       )

!
!     Check for the absence of an exception.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check outputs.
!
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL CHCKSC ( 'FNAME', FNAME, '=', ' ', OK )

      CALL CHCKSI ( 'INTARC', INTARC, '=', 0, 0, OK )
      CALL CHCKSI ( 'INTBFF', INTBFF, '=', 0, 0, OK )
      CALL CHCKSI ( 'INTAMH', INTAMH, '=', 0, 0, OK )

!
! ----------------------------------------------------------------
!
!     Now clean up.
!
      DO I = 1, FILCNT
         CALL ZZDDHCLS ( TBLHAN(I), STRARC(TBLARC(I)), .FALSE. )
         CALL KILFIL   ( TBLFNM(I) )
      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
