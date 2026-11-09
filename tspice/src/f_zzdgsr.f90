!$Procedure F_ZED'S ( ZZDAFGSR Test Family )

      SUBROUTINE F_ZZDGSR ( OK )
      use pepper_inc_zzddhman

! $ Abstract
!
!     Test family to exercise the logic and code in the ZZDAFGSR
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
!     This routine exercises ZZDAFGSR's logic.
!
!     In the process of exercising ZZDAFGSR's logic this module
!     creates a series of illegitmately structured DAF files
!     that contain legitmate summary/descriptor records.  These
!     test files are destroyed after successful execution of this
!     test module.
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
!-    TSPICE Version 1.0.0, 15-OCT-2001 (FST)
!
!
!-&

!
!     Local Variables
!
      CHARACTER*(80)        CASENM
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
      DOUBLE PRECISION      PAARY  (  128   )
      DOUBLE PRECISION      RDREC  (  128   )
      DOUBLE PRECISION      TDC    (  125   )

      INTEGER               BWARD
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               HANLST ( NUMBFF )
      INTEGER               I
      INTEGER               IC     (  250   )
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               M
      INTEGER               MXNSUM
      INTEGER               NATBFF
      INTEGER               NEXT
      INTEGER               NSUM
      INTEGER               NUMSUP
      INTEGER               PREV
      INTEGER               SUMSIZ
      INTEGER               SUPBFF ( NUMBFF )
      INTEGER               TIC    (  250   )
      INTEGER               UNIT

      LOGICAL               ADDFTP
      LOGICAL               FOUND

!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_ZZDGSR' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'F_ZZDGSR Initialization' )

!
!     Retrieve the native format and other related information.
!
      CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, STRAMH, STRARC, STRBFF )

!
!     Setup the filename template.
!
      FNMTMP = 'daf#.daf'

!
!     Construct the contents of the DAF to create.  These values
!     are just to satisfy the arguments of T_DAFWFR, as we are only
!     creating a test file to exercise ZZDAFGSR.
!
      IDWORD = 'DAF/TEST'
      IFNAME = 'TSPICE Test DAF'
      FWARD  = 2
      BWARD  = 2
      FREE   = 128*125+1
      ADDFTP = .TRUE.

!
!     Construct the DAFs for each supported architecture, load them
!     into the handle manager and perform the necessary tests.
!
      DO I = 1, NUMSUP

         CALL REPMI    ( FNMTMP, '#', I, FNAME )

!
!        Now loop over all possible combinations of ND and NI.
!        J ranges over the possible values of ND, while K over NI.
!
!         DO J = 0, 124
!            DO K = 2, 2*(125-J)
!
!        In the interest of increasing execution speed on most of
!        the test platforms only perform the following, commonly
!        used summary tests.
!
         DO J = 2, 2
            DO K = 5, 6

!
!              Declare a new test case, since we are constructing them
!              dynamically.
!
               CASENM = '# reading # data. ND = #, NI = #.'
               CALL REPMC ( CASENM, '#', STRBFF(NATBFF),    CASENM )
               CALL REPMC ( CASENM, '#', STRBFF(SUPBFF(I)), CASENM )
               CALL REPMI ( CASENM, '#', J,                 CASENM )
               CALL REPMI ( CASENM, '#', K,                 CASENM )

               CALL TCASE ( CASENM )

!
!              Open the new DAF.
!
               CALL T_DAFOPN ( FNAME, SUPBFF(I), UNIT )

!
!              Dump the semi-bogus file record into the new DAF.
!
               CALL T_DAFWFR ( UNIT,  SUPBFF(I), IDWORD, J, &
                               K,     IFNAME,    FWARD,  BWARD, &
                               FREE,  ADDFTP                    )


!
!              Compute the maximum number of summaries each summary
!              record can hold for these values of ND and NI.
!
               MXNSUM = 125/(J+(K+1)/2)
               SUMSIZ = J+(K+1)/2

!
!              Now construct the summaries we are going to store
!              in each summary record.  We will saturate ARRAY
!              with a sequence of packed summaries.
!
               DO L = 1, MXNSUM

!
!                 Construct the DP components.
!
                  DO M = 1, J
                     DC(M) = DBLE(L)*DBLE(M)
                  END DO

!
!                 Construct the integer components
!
                  DO M = 1, K
                     IC(M) = L*M
                  END DO

                  CALL DAFPS ( J, K, DC, IC, &
                               ARRAY( (L-1)*SUMSIZ + 1 ) )

               END DO

!
!              Now construct each summary record that is to be
!              written into the DAF.  The first L summaries from
!              ARRAY will be copied into the target record along
!              with bogus values for NEXT, PREV, and L itself as
!              NSUM.  The remainder of the record is zeroed out
!              for the safety of the reverse conversion module's
!              sake.
!
               DO L = 0, MXNSUM

!
!                 We are just going to assign bogus values to NEXT and
!                 PREV, because we are not really going to be reading
!                 this with any of the high level DAF routines.
!
                  NEXT = 2
                  PREV = 2
                  NSUM = L

!
!                 We have the baseline ARRAY of MXNSUM packed
!                 summaries.  Start populating records, by copying
!                 the appropriate number of packed summaries into
!                 PAARY.
!
                  CALL CLEARD ( 128,   PAARY )
                  CALL MOVED  ( ARRAY, L*SUMSIZ, PAARY )

!
!                 Write the summary record to the new DAF.
!
                  CALL T_DAFWSR ( UNIT, L+2,  SUPBFF(I), J,    K, &
                                  NEXT, PREV, NSUM,      PAARY    )

               END DO

!
!              Close the DAF.
!
               CLOSE ( UNIT = UNIT )

!
!              Open the DAF in the handle manager.
!
               CALL ZZDDHOPN ( FNAME, 'READ', 'DAF', HANLST(I) )

!
!              Check for the absence of an exception.
!
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Now check each record in the file we just created.
!
               DO L = 0, MXNSUM

!
!                 Setup the inputs and outputs.
!
                  CALL CLEARI ( 250, TIC )
                  CALL CLEARD ( 125, TDC )

                  FOUND = .FALSE.

!
!                 Invoke the module.
!
                  CALL ZZDAFGSR ( HANLST(I), L+2, J, K, DATA, FOUND )

!
!                 Check for the absence of an exception.
!
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Now check the results. Start by examining the
!                 values stored in the record for NEXT, PREV,
!                 and NSUM.
!
                  CALL CHCKSD ( 'NEXT', DATA(1), '=', DBLE(NEXT), &
                                0.0D0,  OK                        )
                  CALL CHCKSD ( 'PREV', DATA(2), '=', DBLE(PREV), &
                                0.0D0,  OK                        )
                  CALL CHCKSD ( 'NSUM', DATA(3), '=', DBLE(L), &
                                0.0D0,  OK                        )

!
!                 Now verify the contents of each summary.
!
                  DO M = 1, L

!
!                    Unpack the summary for the test array that was
!                    written to the file.
!
                     CALL DAFUS ( ARRAY((M-1)*SUMSIZ+1), &
                                  J, &
                                  K, &
                                  DC, &
                                  IC                   )

!
!                    Unpack the next summary from DATA.
!
                     CALL DAFUS ( DATA(4+(M-1)*SUMSIZ), &
                                  J, &
                                  K, &
                                  TDC, &
                                  TIC                   )

!
!                    Compare the contents of the summaries.
!
                     CALL CHCKAD ( 'DC', TDC, '=', DC, J, 0.0D0, OK )
                     CALL CHCKAI ( 'IC', TIC, '=', IC, K, OK )

                  END DO

               END DO

!
!              Close the file, removing it from the handle manager.
!
               CALL ZZDDHCLS ( HANLST(I), 'DAF', .FALSE. )

!
!              Kill the file, so we can reuse the name safely.
!
               CALL KILFIL ( FNAME )

            END DO
         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'SPICE(HANDLENOTFOUND) Exception' )

!
!     Setup outputs and expected values.
!
      FOUND = .TRUE.

      CALL CLEARD ( 128, DATA )
      CALL CLEARD ( 128, RDREC )

!
!     Since we know we just unloaded HANLST(NUMSUP), attempt to read
!     from that handle.
!
      CALL ZZDAFGSR ( HANLST(NUMSUP), 2, 124, 2, RDREC, FOUND )

!
!     Check for the presence of the exception.
!
      CALL CHCKXC ( .TRUE., 'SPICE(HANDLENOTFOUND)', OK )

!
!     Check outputs. FOUND should be FALSE, and RDREC should be
!     untouched.
!
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL CHCKAD ( 'RDREC', RDREC, '=', DATA, 128, 0.0D0, OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
