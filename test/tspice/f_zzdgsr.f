C$Procedure F_ZED'S ( ZZDAFGSR Test Family )
 
      SUBROUTINE F_ZZDGSR ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the ZZDAFGSR
C     routine.
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
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      LOGICAL               OK
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Particulars
C
C     This routine exercises ZZDAFGSR's logic.
C
C     In the process of exercising ZZDAFGSR's logic this module
C     creates a series of illegitmately structured DAF files
C     that contain legitmate summary/descriptor records.  These
C     test files are destroyed after successful execution of this
C     test module.
C
C$ Examples
C
C     None.
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
C-    TSPICE Version 2.0.0, 07-AUG-2002 (FST)
C
C        Updated module as the result of changes to the handle
C        manager interface, ZZDDHCLS.
C
C-    TSPICE Version 1.0.0, 15-OCT-2001 (FST)
C
C
C-&
 
C
C     Local Variables
C
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
 
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_ZZDGSR' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'F_ZZDGSR Initialization' )
 
C
C     Retrieve the native format and other related information.
C
      CALL ZZDDHINI ( NATBFF, SUPBFF, NUMSUP, STRAMH, STRARC, STRBFF )
 
C
C     Setup the filename template.
C
      FNMTMP = 'daf#.daf'
 
C
C     Construct the contents of the DAF to create.  These values
C     are just to satisfy the arguments of T_DAFWFR, as we are only
C     creating a test file to exercise ZZDAFGSR.
C
      IDWORD = 'DAF/TEST'
      IFNAME = 'TSPICE Test DAF'
      FWARD  = 2
      BWARD  = 2
      FREE   = 128*125+1
      ADDFTP = .TRUE.
 
C
C     Construct the DAFs for each supported architecture, load them
C     into the handle manager and perform the necessary tests.
C
      DO I = 1, NUMSUP
 
         CALL REPMI    ( FNMTMP, '#', I, FNAME )
 
C
C        Now loop over all possible combinations of ND and NI.
C        J ranges over the possible values of ND, while K over NI.
C
C         DO J = 0, 124
C            DO K = 2, 2*(125-J)
C
C        In the interest of increasing execution speed on most of
C        the test platforms only perform the following, commonly
C        used summary tests.
C
         DO J = 2, 2
            DO K = 5, 6
 
C
C              Declare a new test case, since we are constructing them
C              dynamically.
C
               CASENM = '# reading # data. ND = #, NI = #.'
               CALL REPMC ( CASENM, '#', STRBFF(NATBFF),    CASENM )
               CALL REPMC ( CASENM, '#', STRBFF(SUPBFF(I)), CASENM )
               CALL REPMI ( CASENM, '#', J,                 CASENM )
               CALL REPMI ( CASENM, '#', K,                 CASENM )
 
               CALL TCASE ( CASENM )
 
C
C              Open the new DAF.
C
               CALL T_DAFOPN ( FNAME, SUPBFF(I), UNIT )
 
C
C              Dump the semi-bogus file record into the new DAF.
C
               CALL T_DAFWFR ( UNIT,  SUPBFF(I), IDWORD, J,
     .                         K,     IFNAME,    FWARD,  BWARD,
     .                         FREE,  ADDFTP                    )
 
 
C
C              Compute the maximum number of summaries each summary
C              record can hold for these values of ND and NI.
C
               MXNSUM = 125/(J+(K+1)/2)
               SUMSIZ = J+(K+1)/2
 
C
C              Now construct the summaries we are going to store
C              in each summary record.  We will saturate ARRAY
C              with a sequence of packed summaries.
C
               DO L = 1, MXNSUM
 
C
C                 Construct the DP components.
C
                  DO M = 1, J
                     DC(M) = DBLE(L)*DBLE(M)
                  END DO
 
C
C                 Construct the integer components
C
                  DO M = 1, K
                     IC(M) = L*M
                  END DO
 
                  CALL DAFPS ( J, K, DC, IC,
     .                         ARRAY( (L-1)*SUMSIZ + 1 ) )
 
               END DO
 
C
C              Now construct each summary record that is to be
C              written into the DAF.  The first L summaries from
C              ARRAY will be copied into the target record along
C              with bogus values for NEXT, PREV, and L itself as
C              NSUM.  The remainder of the record is zeroed out
C              for the safety of the reverse conversion module's
C              sake.
C
               DO L = 0, MXNSUM
 
C
C                 We are just going to assign bogus values to NEXT and
C                 PREV, because we are not really going to be reading
C                 this with any of the high level DAF routines.
C
                  NEXT = 2
                  PREV = 2
                  NSUM = L
 
C
C                 We have the baseline ARRAY of MXNSUM packed
C                 summaries.  Start populating records, by copying
C                 the appropriate number of packed summaries into
C                 PAARY.
C
                  CALL CLEARD ( 128,   PAARY )
                  CALL MOVED  ( ARRAY, L*SUMSIZ, PAARY )
 
C
C                 Write the summary record to the new DAF.
C
                  CALL T_DAFWSR ( UNIT, L+2,  SUPBFF(I), J,    K,
     .                            NEXT, PREV, NSUM,      PAARY    )
 
               END DO
 
C
C              Close the DAF.
C
               CLOSE ( UNIT = UNIT )
 
C
C              Open the DAF in the handle manager.
C
               CALL ZZDDHOPN ( FNAME, 'READ', 'DAF', HANLST(I) )
 
C
C              Check for the absence of an exception.
C
               CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C              Now check each record in the file we just created.
C
               DO L = 0, MXNSUM
 
C
C                 Setup the inputs and outputs.
C
                  CALL CLEARI ( 250, TIC )
                  CALL CLEARD ( 125, TDC )
 
                  FOUND = .FALSE.
 
C
C                 Invoke the module.
C
                  CALL ZZDAFGSR ( HANLST(I), L+2, J, K, DATA, FOUND )
 
C
C                 Check for the absence of an exception.
C
                  CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C                 Now check the results. Start by examining the
C                 values stored in the record for NEXT, PREV,
C                 and NSUM.
C
                  CALL CHCKSD ( 'NEXT', DATA(1), '=', DBLE(NEXT),
     .                          0.0D0,  OK                        )
                  CALL CHCKSD ( 'PREV', DATA(2), '=', DBLE(PREV),
     .                          0.0D0,  OK                        )
                  CALL CHCKSD ( 'NSUM', DATA(3), '=', DBLE(L),
     .                          0.0D0,  OK                        )
 
C
C                 Now verify the contents of each summary.
C
                  DO M = 1, L
 
C
C                    Unpack the summary for the test array that was
C                    written to the file.
C
                     CALL DAFUS ( ARRAY((M-1)*SUMSIZ+1),
     .                            J,
     .                            K,
     .                            DC,
     .                            IC                   )
 
C
C                    Unpack the next summary from DATA.
C
                     CALL DAFUS ( DATA(4+(M-1)*SUMSIZ),
     .                            J,
     .                            K,
     .                            TDC,
     .                            TIC                   )
 
C
C                    Compare the contents of the summaries.
C
                     CALL CHCKAD ( 'DC', TDC, '=', DC, J, 0.0D0, OK )
                     CALL CHCKAI ( 'IC', TIC, '=', IC, K, OK )
 
                  END DO
 
               END DO
 
C
C              Close the file, removing it from the handle manager.
C
               CALL ZZDDHCLS ( HANLST(I), 'DAF', .FALSE. )
 
C
C              Kill the file, so we can reuse the name safely.
C
               CALL KILFIL ( FNAME )
 
            END DO
         END DO
 
      END DO
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPICE(HANDLENOTFOUND) Exception' )
 
C
C     Setup outputs and expected values.
C
      FOUND = .TRUE.
 
      CALL CLEARD ( 128, DATA )
      CALL CLEARD ( 128, RDREC )
 
C
C     Since we know we just unloaded HANLST(NUMSUP), attempt to read
C     from that handle.
C
      CALL ZZDAFGSR ( HANLST(NUMSUP), 2, 124, 2, RDREC, FOUND )
 
C
C     Check for the presence of the exception.
C
      CALL CHCKXC ( .TRUE., 'SPICE(HANDLENOTFOUND)', OK )
 
C
C     Check outputs. FOUND should be FALSE, and RDREC should be
C     untouched.
C
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
      CALL CHCKAD ( 'RDREC', RDREC, '=', DATA, 128, 0.0D0, OK )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
