!$Procedure      T_GFTREP ( GF, progress report test utilities )

      SUBROUTINE T_GFTREP ( CNFINE, MSGPRE, MSGSUF, IVBEG,  IVEND, &
                            ET,     NMAX,   MW,     NCALLS, CNFLOG, &
                            MSGLOG, REPLOG, SEQLOG                 )

! $ Abstract
!
!     Umbrella routine for GF progress report test utilities.
!
! $ Required_Reading
!
!     GF
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               WINMAX
      PARAMETER           ( WINMAX = 20000 )

      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      CHARACTER*(*)         MSGPRE
      CHARACTER*(*)         MSGSUF
      DOUBLE PRECISION      IVBEG
      DOUBLE PRECISION      IVEND
      DOUBLE PRECISION      ET
      INTEGER               NMAX
      INTEGER               MW
      INTEGER               NCALLS
      DOUBLE PRECISION      CNFLOG ( LBCELL : MW, * )
      CHARACTER*(*)         MSGLOG ( 2, * )
      DOUBLE PRECISION      REPLOG ( 3, * )
      INTEGER               SEQLOG ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     CNFINE     I   T_GFREPI
!     MSGPRE     I   T_GFREPI
!     MSGSUF     I   T_GFREPI
!     IVBEG      I   T_GFREPU
!     IVEND      I   T_GFREPU
!     ET         I   T_GFREPU
!     NMAX       I   T_GFRINI, T_GFRPLO, T_GFRTRM
!     MW         I   T_GFRINI
!     NCALLS     O   T_GFRINI, T_GFRPLO, T_GFRTRM
!     CNFLOG     O   T_GFRINI
!     MSGLOG     O   T_GFRINI
!     REPLOG     O   T_GFRPLO
!     SEQLOG     O   T_GFRINI, T_GFRPLO, T_GFRTRM
!
! $ Detailed_Input
!
!     See the entry points.
!
! $ Detailed_Output
!
!     See the entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If this routine is called directly, the error SPICE(BOGUSENTRY)
!        is signaled.
!
!     See the entry points for discussions of exceptions handled by
!     those routines.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This test utility package enables TSPICE to verify that the GF
!     system makes correct calls to either the default progress
!     reporting routines
!
!        GFREPI
!        GFREPU
!        GFREPF
!
!     or to their user-supplied replacements. These utilities
!     support logging of and retrieval of arguments passed to
!     progress reporting routines.
!
!     This umbrella routine contains the following entry points:

!        T_GFUINI ( Utility initialization )
!        T_GFREPI ( GFREPI replacement )
!        T_GFREPU ( GFREPU replacement )
!        T_GFREPF ( GFREPF replacement )
!        T_GFRINI ( return progress report initial values )
!        T_GFRPLO ( return progress report log )
!        T_GFRTRM ( return termination log )
!
! $ Examples
!
!     See usage in F_GFDIRP.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Fixed typo in T_GFUINI entry point Procedure section.
!        Corrected Version strings in module.
!
!-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
!
!-&

! $ Index_Entries
!
!     GF progress report test utilities
!
!-&


!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               MSGMAX
      PARAMETER           ( MSGMAX = 32 )

      INTEGER               MXPASS
      PARAMETER           ( MXPASS = 20 )

      INTEGER               MAXLOG
      PARAMETER           ( MAXLOG = 100000 )

!
!     Local variables
!
      INTEGER               I

      CHARACTER*(MSGMAX)    SVMSLG ( 2, MXPASS )

      DOUBLE PRECISION      SVCNFN ( LBCELL : WINMAX, MXPASS )
      DOUBLE PRECISION      SVULOG ( 3, MAXLOG )

      INTEGER               SVFSEQ ( MXPASS )
      INTEGER               SVISEQ ( MXPASS )
      INTEGER               SVNUPD
      INTEGER               SVNFIN
      INTEGER               SVNINI
      INTEGER               SVSQIX
      INTEGER               SVUSEQ ( MAXLOG )

!
!     Saved variables
!
!
!     Save everything.
!
      SAVE

      CALL CHKIN  ( 'T_GFTREP'          )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_GFTREP'          )

      RETURN





!$Procedure  T_GFUINI ( GF, progress report test utility init )

      ENTRY T_GFUINI

! $ Abstract
!
!     Initialize GF progress report test utilities.
!
! $ Required_Reading
!
!     GF
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!
!     None.
!
! $ Detailed_Input
!
!     None.
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
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine sets initial values for saved data shared
!     by routines in this test utility package.
!
!     This routine should be called prior to each GF search
!     conducted by any test program using these utilities.
!
! $ Examples
!
!     See usage in F_GFDIRP.
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
!     N.J. Bachman      (JPL)
!     J. Diaz del Rio   (ODC Space)
!     E.D. Wright       (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
!
!        Fixed typo in Procedure section.
!
!-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
!
!-&

! $ Index_Entries
!
!     GF progress report test utility initialization
!
!-&

      CALL CHKIN  ( 'T_GFUINI' )


      SVNINI = 0
      SVNUPD = 0
      SVNFIN = 0

      CALL CLEARD ( 3*MAXLOG, SVULOG )

      DO I = 1, MXPASS

         CALL SSIZED ( WINMAX, SVCNFN(LBCELL,I) )

         SVMSLG(1,I) = ' '
         SVMSLG(2,I) = ' '

      END DO

      SVSQIX = 0

      CALL CLEARI ( MXPASS, SVISEQ )
      CALL CLEARI ( MAXLOG, SVUSEQ )
      CALL CLEARI ( MXPASS, SVFSEQ )


      CALL CHKOUT ( 'T_GFUINI' )
      RETURN



!$Procedure  T_GFREPI ( GF, GFREPI stand-in )

      ENTRY T_GFREPI ( CNFINE, MSGPRE, MSGSUF )

! $ Abstract
!
!     This is an instrumented replacement for GFREPI: this routine
!     logs its inputs so they may be examined after the completion
!     of a GF search.
!
! $ Required_Reading
!
!     GF
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      CNFINE ( LBCELL : * )
!     CHARACTER*(*)         MSGPRE
!     CHARACTER*(*)         MSGSUF
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CNFINE     I   Confinement window.
!     MSGPRE     I   Message prefix.
!     MSGSUF     I   Message suffix.
!
! $ Detailed_Input
!
!     CNFINE     is the confinement window over which a GF search
!                is to be performed.
!
!     MSGPRE     is a progress report prefix.
!
!     MSGSUF     is a progress report suffix.
!
! $ Detailed_Output
!
!     None. See Particulars.
!
! $ Parameters
!
!     MXPASS     is the maximum number of calls that can be made to
!                this routine during one GF search. Normally this
!                routine is called once per search pass.
!
! $ Exceptions
!
!     1) If the input confinement window has cardinality larger
!        than the size of the windows in the confinement window
!        buffer, the error will be diagnosed by routines in
!        the call tree of this routine.
!
!     2) If more than MXPASS calls are made to this routine
!        since the last call to T_GFUINI, the error SPICE(OVERFLOW)
!        is signaled.
!
!     3) If this routine is called before the initialization
!        routine T_GFUINI is called, the behavior of this
!        system will be undefined.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine should be used in place of GFREPI by test families
!     that examine progress reports created by mid-level GF search
!     routines.
!
!     Values stored by this routine may be retrieved by calling
!
!        T_GFRINI
!
! $ Examples
!
!     See usage in F_GFDIRP.
!
! $ Restrictions
!
!     1) This routine relies on fixed dimension buffers. See the
!        implementation and declarations in the umbrella routine
!        for details.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!     E.D. Wright       (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
!
!-&

! $ Index_Entries
!
!     GF progress report initialization stand-in
!
!-&

      CALL CHKIN ( 'T_GFREPI' )

!
!     Increment the progress report initialization count.
!
      SVNINI = SVNINI + 1

      IF ( SVNINI .GT. MXPASS ) THEN

         CALL SETMSG ( 'Too many initialization calls: ' &
         //            'number of progress reporting initialization ' &
         //            'calls (to T_GRREPI) since setup is #; room ' &
         //            'in static T_GFTREP buffers is #.'            )
         CALL ERRINT ( '#', SVNINI                                   )
         CALL ERRINT ( '#', MXPASS                                   )
         CALL SIGERR ( 'SPICE(OVERFLOW)'                             )
         CALL CHKOUT ( 'T_GFREPI'                                    )
         RETURN

      END IF

!
!     Increment the call sequence index, and save this
!     index in the initialization sequence log.
!
      SVSQIX            = SVSQIX + 1
      SVISEQ ( SVNINI ) = SVSQIX

!
!     Store the inputs in the initialization log.
!
      CALL COPYD ( CNFINE, SVCNFN(LBCELL,SVNINI) )

      SVMSLG ( 1, SVNINI ) = MSGPRE
      SVMSLG ( 2, SVNINI ) = MSGSUF

      CALL CHKOUT ( 'T_GFREPI' )
      RETURN





!$Procedure  T_GFRINI ( GF, return values passed to T_GFREPI )

      ENTRY T_GFRINI ( NMAX, MW, NCALLS, SEQLOG, CNFLOG, MSGLOG )

! $ Abstract
!
!     Return values passed to T_GFREPI during a search, along with
!     call sequence indices.
!
! $ Required_Reading
!
!     GF
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     INTEGER               NMAX
!     INTEGER               MW
!     INTEGER               NCALLS
!     INTEGER               SEQLOG ( * )
!     DOUBLE PRECISION      CNFLOG ( LBCELL : MW, * )
!     CHARACTER*(*)         MSGLOG ( 2, * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NMAX       I   Maximum number of log entries to return.
!     MW         I   Confinement window size.
!     NCALLS     O   Number of T_GFREPI calls made during run.
!     SEQLOG     O   Log of T_GFREPI call sequence numbers.
!     CNFLOG     O   Log of confinement windows.
!     MSGLOG     O   Log of progress report messages.
!
! $ Detailed_Input
!
!     NMAX       is the maximum number of log records that can be
!                accommodated in the output arrays. NMAX should always
!                be set to a value at least equal to MXPASS.
!
!     MW         is the declared length of the windows in the
!                output confinement window log.
!
!
! $ Detailed_Output
!
!     NCALLS     is the number of calls to T_GFREPI made during a
!                search. This count is reset to 0 every time the
!                utility initialization entry point T_GFUINI is
!                called.
!
!     SEQLOG     is a log of sequence numbers of the calls to
!                the routine T_GFREPI made during a test run. Sequence
!                numbers start at one and are monotone increasing.
!                Sequence numbers of the calls to to T_GFREPI,
!                T_GFREPU, and T_GFREPF form a consecutive sequence.
!                Normal GF progress reporting involves a sequence of
!                calls, for each search pass, having the pattern:
!
!                   T_GFREPI (once)
!                   T_GFREPU (multiple calls)
!                   T_GFREPF (once)
!
!     CNFLOG    is a log of confinement windows passed to T_GFREPI
!               during a search. This array has dimensions
!
!                  ( LBCELL : MW, * )
!
!     MSGLOG    is a log of progress report prefix and suffix
!               messages. One pair of messages is stored for each
!               call made to T_GFREPI during a search pass.
!
!
! $ Parameters
!
!     MXPASS     is the maximum number of calls that can be made to
!                T_GFREPI during one GF search. Normally T_GFREPI
!                is called once per search pass.
!
! $ Exceptions
!
!     1) If NMAX is smaller than the declared length of the
!        static arrays used by this routine to store report
!        logs, the error SPICE(BUG) will be signaled.
!
!     2) If this routine is called before the initialization
!        routine T_GFUINI is called, the behavior of this
!        system will be undefined.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine enables checking of the progress report confinement
!     windows and messages passed into the progress report
!     initialization routine T_GFREPI.
!
! $ Examples
!
!     See usage in F_GFDIRP.
!
! $ Restrictions
!
!     1) This routine relies on fixed dimension buffers. See the
!        implementation and declarations in the umbrella routine
!        for details.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
!
!-&

! $ Index_Entries
!
!     return GF progress report initialization log
!
!-&

      CALL CHKIN ( 'T_GFRINI' )

!
!     Make sure we have room for the output.
!
      IF ( SVNINI .GT. NMAX ) THEN

         CALL SETMSG ( 'Number of progress reporting initialization ' &
         //            'calls (to T_GRREPI) since setup is #; room ' &
         //            'in output buffer is #.'                      )
         CALL ERRINT ( '#', SVNINI                                   )
         CALL ERRINT ( '#', NMAX                                     )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'T_GFRINI'                                    )
         RETURN

      END IF

!
!     Set the outputs:
!
!        - Number of calls to T_GFREPI
!        - Sequence numbers of the calls to T_GFREPI
!        - Input confinement windows
!        - Input messages
!

      NCALLS = SVNINI

      CALL MOVEI ( SVISEQ, SVNINI, SEQLOG )

      DO I = 1, SVNINI

         CALL COPYD ( SVCNFN(LBCELL,I), CNFLOG(LBCELL,I) )

         MSGLOG(1,I) = SVMSLG(1,I)
         MSGLOG(2,I) = SVMSLG(2,I)

      END DO

      CALL CHKOUT ( 'T_GFRINI' )
      RETURN





!$Procedure  T_GFREPU ( GF, GFREPU stand-in )

      ENTRY T_GFREPU ( IVBEG, IVEND, ET )

! $ Abstract
!
!     This is an instrumented replacement for GFREPU: this routine
!     logs its inputs so they may be examined after the completion
!     of a GF search.
!
! $ Required_Reading
!
!     GF
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      IVBEG
!     DOUBLE PRECISION      IVEND
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IVBEG      I   Interval start time.
!     IVEND      I   Interval end time.
!     ET         I   Current time.
!
! $ Detailed_Input
!
!     IVBEG,
!     IVEND      are, respectively, the start and stop time of an
!                interval traversed during progress reporting.
!
!     ET         is a time within the interval [IVBEG, IVEND].
!
! $ Detailed_Output
!
!     None. See Particulars.
!
! $ Parameters
!
!     MAXLOG     is the maximum number of calls that can be made to
!                this routine during one search. Normally this
!                routine is called multiple times per GF search pass.
!
! $ Exceptions
!
!     1) If more than MAXLOG calls are made to this routine
!        since the last call to T_GFUINI, the error SPICE(OVERFLOW)
!        is signaled.
!
!     2) If this routine is called before the initialization
!        routine T_GFUINI is called, the behavior of this
!        system will be undefined.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine should be used in place of GFREPU by test families
!     that examine progress reports created by mid-level GF search
!     routines.
!
!     Values stored by this routine may be retrieved by calling
!
!        T_GFRPLO
!
! $ Examples
!
!     See usage in F_GFDIRP.
!
! $ Restrictions
!
!     1) This routine relies on fixed dimension buffers. See the
!        implementation and declarations in the umbrella routine
!        for details.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
!
!-&

! $ Index_Entries
!
!     GF progress report update stand-in
!
!-&

      CALL CHKIN ( 'T_GFREPU' )

!
!     Increment the progress report update count.
!
      SVNUPD = SVNUPD + 1


      IF ( SVNUPD .GT. MAXLOG ) THEN

         CALL SETMSG ( 'Too many update calls: ' &
         //            'number of progress reporting update ' &
         //            'calls (to T_GRREPU) since setup is #; room ' &
         //            'in static T_GFTREP buffers is #.'            )
         CALL ERRINT ( '#', SVNUPD                                   )
         CALL ERRINT ( '#', MAXLOG                                   )
         CALL SIGERR ( 'SPICE(OVERFLOW)'                             )
         CALL CHKOUT ( 'T_GFREPU'                                    )
         RETURN

      END IF

!
!     Increment the call sequence index, and save this
!     index in the update sequence log.
!
      SVSQIX = SVSQIX + 1


      SVUSEQ ( SVNUPD ) = SVSQIX

!
!     Store the inputs in the update log.
!
      SVULOG ( 1, SVNUPD ) = IVBEG
      SVULOG ( 2, SVNUPD ) = IVEND
      SVULOG ( 3, SVNUPD ) = ET

      CALL CHKOUT ( 'T_GFREPU' )
      RETURN






!$Procedure  T_GFRPLO ( GF, return values passed to T_GFREPU )

      ENTRY T_GFRPLO ( NMAX, NCALLS, SEQLOG, REPLOG )

! $ Abstract
!
!
!     Return values passed to T_GFREPU, along with call sequence
!     indices.
!
! $ Required_Reading
!
!     GF
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     INTEGER               NMAX
!     INTEGER               NCALLS
!     INTEGER               SEQLOG ( * )
!     DOUBLE PRECISION      REPLOG ( 3, * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NMAX       I   Maximum number of log entries to return.
!     NCALLS     O   Number of T_GFREPI calls made during run.
!     SEQLOG     O   Log of T_GFREPI call sequence numbers.
!     REPLOG     O   Log of progress update intervals and times.
!
! $ Detailed_Input
!
!     NMAX       is the maximum number of log records that can be
!                accommodated in the output arrays. NMAX should always
!                be set to a value at least equal to MAXLOG.
!
! $ Detailed_Output
!
!     NCALLS     is the number of calls to T_GFREPI made during a
!                search. This count is reset to 0 every time the
!                utility initialization entry point T_GFUINI is
!                called.
!
!     SEQLOG     is a log of sequence numbers of the calls to
!                the routine T_GFREPU made during a test run. Sequence
!                numbers start at one and are monotone increasing.
!                Sequence numbers of the calls to to T_GFREPI,
!                T_GFREPU, and T_GFREPF form a consecutive sequence.
!                Normal GF progress reporting involves a sequence of
!                calls, for each search pass, having the pattern:
!
!                   T_GFREPI (once)
!                   T_GFREPU (multiple calls)
!                   T_GFREPF (once)
!
!     REPLOG    is a log of update intervals and times passed to
!               T_GFREPU. The Ith row of this array contains the
!               values passed to T_GFREPU on the Ith call since
!               the last utility initialization call. Within
!               the Ith row of this array, the consecutive elements
!               contain the values of IVBEG, IVEND, and ET passed
!               into T_GFPREP on the Ith call to T_GFREPU since
!               the last utility initialization call.
!
!
! $ Parameters
!
!     MAXLOG     is the maximum number of calls that can be made to
!                T_GFREPU during a search. Normally T_GFREPU is
!                called multiple times during one GF search pass.
!
! $ Exceptions
!
!     1) If NMAX is smaller than the declared length of the
!        static arrays used by this routine to store report
!        logs, the error SPICE(BUG) will be signaled.
!
!     2) If this routine is called before the initialization
!        routine T_GFUINI is called, the behavior of this
!        system will be undefined.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine enables checking of the progress report confinement
!     windows and messages passed into the progress report
!     initialization routine T_GFREPU.
!
! $ Examples
!
!     See usage in F_GFDIRP.
!
! $ Restrictions
!
!     1) This routine relies on fixed dimension buffers. See the
!        implementation and declarations in the umbrella routine
!        for details.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
!
!-&

! $ Index_Entries
!
!     return GF progress report update log
!
!-&


      CALL CHKIN ( 'T_GFRPLO' )

!
!     Make sure we have room for the output.
!
      IF ( SVNUPD .GT. NMAX ) THEN

         CALL SETMSG ( 'Number of progress reporting update ' &
         //            'calls (to T_GRREPU) since setup is #; room ' &
         //            'in buffer is #.'                             )
         CALL ERRINT ( '#', SVNUPD                                   )
         CALL ERRINT ( '#', NMAX                                     )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'T_GFRPLO'                                    )
         RETURN

      END IF

!
!     Set the outputs:
!
!        - Number of calls to T_GFREPU
!
!        - Sequence numbers of the calls to T_GFREPU
!
!        - Update log, which contains
!
!            > interval begin times
!            > interval end times
!            > ET values
!

      NCALLS = SVNUPD

      CALL MOVEI ( SVUSEQ, SVNUPD, SEQLOG )

      DO I = 1, SVNUPD

         REPLOG(1,I) = SVULOG(1,I)
         REPLOG(2,I) = SVULOG(2,I)
         REPLOG(3,I) = SVULOG(3,I)

      END DO

      CALL CHKOUT ( 'T_GFRPLO' )
      RETURN






!$Procedure  T_GFREPF ( GF, GFREPF stand-in )

      ENTRY T_GFREPF

! $ Abstract
!
!     This is an instrumented replacement for GFREPF: this routine logs
!     the sequence numbers of calls made to it so they may be examined
!     after the completion of a GF search.
!
! $ Required_Reading
!
!     GF
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None. See Particulars.
!
! $ Parameters
!
!     MAXLOG     is the maximum number of calls that can be made to
!                this routine during one GF search.
!
! $ Exceptions
!
!     1) If more than MAXLOG calls are made to this routine
!        since the last call to T_GFUINI, the error SPICE(OVERFLOW)
!        is signaled.
!
!     2) If this routine is called before the initialization
!        routine T_GFUINI is called, the behavior of this
!        system will be undefined.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine should be used in place of GFREPF by test families
!     that examine progress reports created by mid-level GF search
!     routines.
!
!     Values stored by this routine may be retrieved by calling
!
!        T_GFRTRM
!
! $ Examples
!
!     See usage in F_GFDIRP.
!
! $ Restrictions
!
!     1) This routine relies on fixed dimension buffers. See the
!        implementation and declarations in the umbrella routine
!        for details.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
!
!-&

! $ Index_Entries
!
!     GF progress report termination stand-in
!
!-&

      CALL CHKIN ( 'T_GFREPF' )

!
!     Increment the progress report termination count.
!
      SVNFIN = SVNFIN + 1


      IF ( SVNFIN .GT. MAXLOG ) THEN

         CALL SETMSG ( 'Too many termination calls: ' &
         //            'number of progress reporting termination ' &
         //            'calls (to T_GRREPF) since setup is #; room ' &
         //            'in static T_GFTREP buffers is #.'            )
         CALL ERRINT ( '#', SVNFIN                                   )
         CALL ERRINT ( '#', MAXLOG                                   )
         CALL SIGERR ( 'SPICE(OVERFLOW)'                             )
         CALL CHKOUT ( 'T_GFREPF'                                    )
         RETURN

      END IF


!
!     Increment the call sequence index, and save this
!     index in the termination sequence log.
!
      SVSQIX            = SVSQIX + 1
      SVFSEQ ( SVNFIN ) = SVSQIX

      CALL CHKOUT ( 'T_GFREPF' )
      RETURN







!$Procedure  T_GFRTRM ( GF, return values passed to T_GFREPU )

      ENTRY T_GFRTRM ( NMAX, NCALLS, SEQLOG )

! $ Abstract
!
!     Return call sequence indices for T_GFREPF calls.
!
! $ Required_Reading
!
!     GF
!     TIME
!
! $ Keywords
!
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations
!
!     INTEGER               NMAX
!     INTEGER               NCALLS
!     INTEGER               SEQLOG ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NMAX       I   Maximum number of log entries to return.
!     NCALLS     O   Number of T_GFREPF calls made during run.
!     SEQLOG     O   Log of T_GFREPF call sequence numbers.
!
! $ Detailed_Input
!
!     NMAX       is the maximum number of log records that can be
!                accommodated in the output arrays. NMAX should always
!                be set to a value at least equal to MXPASS.
!
! $ Detailed_Output
!
!     NCALLS     is the number of calls to T_GFREPF made during a
!                search. This count is reset to 0 every time the
!                utility initialization entry point T_GFUINI is
!                called.
!
!     SEQLOG     is a log of sequence numbers of the calls to
!                the routine T_GFREPF made during a test run. Sequence
!                numbers start at one and are monotone increasing.
!                Sequence numbers of the calls to to T_GFREPI,
!                T_GFREPU, and T_GFREPF form a consecutive sequence.
!                Normal GF progress reporting involves a sequence of
!                calls, for each search pass, having the pattern:
!
!                   T_GFREPI (once)
!                   T_GFREPU (multiple calls)
!                   T_GFREPF (once)
!
!
! $ Parameters
!
!     MXPASS     is the maximum number of calls that can be made to
!                T_GFREPF during a search. Normally T_GFREPF is
!                called once per GF search pass.
!
! $ Exceptions
!
!     1) If NMAX is smaller than the declared length of the
!        static arrays used by this routine to store report
!        logs, the error SPICE(BUG) will be signaled.
!
!     2) If this routine is called before the initialization
!        routine T_GFUINI is called, the behavior of this
!        system will be undefined.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine enables checking of the sequence of calls made to
!     the progress report termination routine T_GFREPF.
!
! $ Examples
!
!     See usage in F_GFDIRP.
!
! $ Restrictions
!
!     1) This routine relies on fixed dimension buffers. See the
!        implementation and declarations in the umbrella routine
!        for details.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
!
!-&

! $ Index_Entries
!
!     return GF progress report termination log
!
!-&


      CALL CHKIN ( 'T_GFRTRM' )

!
!     Make sure we have room for the output.
!
      IF ( SVNFIN .GT. NMAX ) THEN

         CALL SETMSG ( 'Number of progress reporting termination ' &
         //            'calls (to T_GRREPF) since setup is #; room ' &
         //            'in buffer is #.'                             )
         CALL ERRINT ( '#', SVNFIN                                   )
         CALL ERRINT ( '#', NMAX                                     )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'T_GFRTRM'                                    )
         RETURN

      END IF

!
!     Set the outputs:
!
!        - Number of calls to T_GFRFIN
!
!        - Sequence numbers of the calls to T_GFRFIN
!

      NCALLS = SVNFIN

      CALL MOVEI ( SVFSEQ, SVNFIN, SEQLOG )

      CALL CHKOUT ( 'T_GFRTRM' )
      RETURN
      END







