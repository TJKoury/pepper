C$Procedure      T_GFTREP ( GF, progress report test utilities )
 
      SUBROUTINE T_GFTREP ( CNFINE, MSGPRE, MSGSUF, IVBEG,  IVEND,
     .                      ET,     NMAX,   MW,     NCALLS, CNFLOG, 
     .                      MSGLOG, REPLOG, SEQLOG                 )

C$ Abstract
C
C     Umbrella routine for GF progress report test utilities.
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
C     GF
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations

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
 
C$ Brief_I/O
C
C     VARIABLE  I/O  ENTRY POINTS
C     --------  ---  --------------------------------------------------
C     CNFINE     I   T_GFREPI
C     MSGPRE     I   T_GFREPI
C     MSGSUF     I   T_GFREPI
C     IVBEG      I   T_GFREPU
C     IVEND      I   T_GFREPU
C     ET         I   T_GFREPU
C     NMAX       I   T_GFRINI, T_GFRPLO, T_GFRTRM
C     MW         I   T_GFRINI
C     NCALLS     O   T_GFRINI, T_GFRPLO, T_GFRTRM
C     CNFLOG     O   T_GFRINI
C     MSGLOG     O   T_GFRINI
C     REPLOG     O   T_GFRPLO
C     SEQLOG     O   T_GFRINI, T_GFRPLO, T_GFRTRM
C
C$ Detailed_Input    
C 
C     See the entry points.
C
C$ Detailed_Output
C
C     See the entry points.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If this routine is called directly, the error SPICE(BOGUSENTRY)
C        is signaled.
C
C     See the entry points for discussions of exceptions handled by
C     those routines.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This test utility package enables TSPICE to verify that the GF
C     system makes correct calls to either the default progress
C     reporting routines
C
C        GFREPI
C        GFREPU
C        GFREPF
C
C     or to their user-supplied replacements. These utilities
C     support logging of and retrieval of arguments passed to
C     progress reporting routines.
C
C     This umbrella routine contains the following entry points:

C        T_GFUINI ( Utility initialization )
C        T_GFREPI ( GFREPI replacement )
C        T_GFREPU ( GFREPU replacement )
C        T_GFREPF ( GFREPF replacement )
C        T_GFRINI ( return progress report initial values )
C        T_GFRPLO ( return progress report log )
C        T_GFRTRM ( return termination log )
C
C$ Examples
C
C     See usage in F_GFDIRP.
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Fixed typo in T_GFUINI entry point Procedure section.
C        Corrected Version strings in module.
C
C-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
C
C-&
 
C$ Index_Entries
C
C     GF progress report test utilities
C
C-&


C
C     SPICELIB functions
C

C
C     Local parameters
C
      INTEGER               MSGMAX
      PARAMETER           ( MSGMAX = 32 )

      INTEGER               MXPASS
      PARAMETER           ( MXPASS = 20 )

      INTEGER               MAXLOG
      PARAMETER           ( MAXLOG = 100000 )

C
C     Local variables
C
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

C
C     Saved variables
C
C
C     Save everything.
C
      SAVE

      CALL CHKIN  ( 'T_GFTREP'          )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_GFTREP'          )

      RETURN





C$Procedure  T_GFUINI ( GF, progress report test utility init )
 
      ENTRY T_GFUINI

C$ Abstract
C
C     Initialize GF progress report test utilities.
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
C     GF
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C
C     None.
C
C$ Detailed_Input    
C 
C     None.
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
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine sets initial values for saved data shared
C     by routines in this test utility package.
C
C     This routine should be called prior to each GF search
C     conducted by any test program using these utilities.
C
C$ Examples
C
C     See usage in F_GFDIRP.
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
C     N.J. Bachman      (JPL)
C     J. Diaz del Rio   (ODC Space)
C     E.D. Wright       (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 07-FEB-2020 (JDR)
C
C        Fixed typo in Procedure section.
C
C-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
C
C-&
 
C$ Index_Entries
C
C     GF progress report test utility initialization
C
C-&

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



C$Procedure  T_GFREPI ( GF, GFREPI stand-in )
 
      ENTRY T_GFREPI ( CNFINE, MSGPRE, MSGSUF )

C$ Abstract
C
C     This is an instrumented replacement for GFREPI: this routine
C     logs its inputs so they may be examined after the completion
C     of a GF search.
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
C     GF
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      CNFINE ( LBCELL : * )
C     CHARACTER*(*)         MSGPRE
C     CHARACTER*(*)         MSGSUF
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     CNFINE     I   Confinement window.
C     MSGPRE     I   Message prefix.
C     MSGSUF     I   Message suffix.
C
C$ Detailed_Input    
C 
C     CNFINE     is the confinement window over which a GF search
C                is to be performed.
C
C     MSGPRE     is a progress report prefix.
C 
C     MSGSUF     is a progress report suffix.
C
C$ Detailed_Output
C
C     None. See Particulars.
C
C$ Parameters
C
C     MXPASS     is the maximum number of calls that can be made to
C                this routine during one GF search. Normally this
C                routine is called once per search pass.
C
C$ Exceptions
C
C     1) If the input confinement window has cardinality larger
C        than the size of the windows in the confinement window
C        buffer, the error will be diagnosed by routines in
C        the call tree of this routine.
C
C     2) If more than MXPASS calls are made to this routine
C        since the last call to T_GFUINI, the error SPICE(OVERFLOW)
C        is signaled.
C
C     3) If this routine is called before the initialization
C        routine T_GFUINI is called, the behavior of this
C        system will be undefined.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine should be used in place of GFREPI by test families
C     that examine progress reports created by mid-level GF search
C     routines.
C
C     Values stored by this routine may be retrieved by calling
C     
C        T_GFRINI
C
C$ Examples
C
C     See usage in F_GFDIRP.
C
C$ Restrictions
C
C     1) This routine relies on fixed dimension buffers. See the 
C        implementation and declarations in the umbrella routine
C        for details.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman      (JPL)
C     E.D. Wright       (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
C
C-&
 
C$ Index_Entries
C
C     GF progress report initialization stand-in
C
C-&

      CALL CHKIN ( 'T_GFREPI' )

C
C     Increment the progress report initialization count.
C
      SVNINI = SVNINI + 1

      IF ( SVNINI .GT. MXPASS ) THEN

         CALL SETMSG ( 'Too many initialization calls: '
     .   //            'number of progress reporting initialization '
     .   //            'calls (to T_GRREPI) since setup is #; room '
     .   //            'in static T_GFTREP buffers is #.'            )
         CALL ERRINT ( '#', SVNINI                                   )
         CALL ERRINT ( '#', MXPASS                                   )
         CALL SIGERR ( 'SPICE(OVERFLOW)'                             )
         CALL CHKOUT ( 'T_GFREPI'                                    )
         RETURN

      END IF

C
C     Increment the call sequence index, and save this
C     index in the initialization sequence log.
C
      SVSQIX            = SVSQIX + 1
      SVISEQ ( SVNINI ) = SVSQIX

C
C     Store the inputs in the initialization log.
C
      CALL COPYD ( CNFINE, SVCNFN(LBCELL,SVNINI) )

      SVMSLG ( 1, SVNINI ) = MSGPRE
      SVMSLG ( 2, SVNINI ) = MSGSUF

      CALL CHKOUT ( 'T_GFREPI' )
      RETURN





C$Procedure  T_GFRINI ( GF, return values passed to T_GFREPI )
 
      ENTRY T_GFRINI ( NMAX, MW, NCALLS, SEQLOG, CNFLOG, MSGLOG )

C$ Abstract
C
C     Return values passed to T_GFREPI during a search, along with 
C     call sequence indices.
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
C     GF
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     INTEGER               NMAX
C     INTEGER               MW
C     INTEGER               NCALLS
C     INTEGER               SEQLOG ( * )
C     DOUBLE PRECISION      CNFLOG ( LBCELL : MW, * )
C     CHARACTER*(*)         MSGLOG ( 2, * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NMAX       I   Maximum number of log entries to return.
C     MW         I   Confinement window size.
C     NCALLS     O   Number of T_GFREPI calls made during run.
C     SEQLOG     O   Log of T_GFREPI call sequence numbers.
C     CNFLOG     O   Log of confinement windows.
C     MSGLOG     O   Log of progress report messages.
C
C$ Detailed_Input    
C 
C     NMAX       is the maximum number of log records that can be 
C                accommodated in the output arrays. NMAX should always
C                be set to a value at least equal to MXPASS.
C
C     MW         is the declared length of the windows in the 
C                output confinement window log. 
C
C
C$ Detailed_Output
C
C     NCALLS     is the number of calls to T_GFREPI made during a 
C                search. This count is reset to 0 every time the 
C                utility initialization entry point T_GFUINI is 
C                called.
C
C     SEQLOG     is a log of sequence numbers of the calls to
C                the routine T_GFREPI made during a test run. Sequence
C                numbers start at one and are monotone increasing.
C                Sequence numbers of the calls to to T_GFREPI,
C                T_GFREPU, and T_GFREPF form a consecutive sequence.
C                Normal GF progress reporting involves a sequence of
C                calls, for each search pass, having the pattern:
C
C                   T_GFREPI (once)
C                   T_GFREPU (multiple calls)
C                   T_GFREPF (once)
C
C     CNFLOG    is a log of confinement windows passed to T_GFREPI
C               during a search. This array has dimensions
C
C                  ( LBCELL : MW, * )
C
C     MSGLOG    is a log of progress report prefix and suffix 
C               messages. One pair of messages is stored for each
C               call made to T_GFREPI during a search pass.
C
C
C$ Parameters
C
C     MXPASS     is the maximum number of calls that can be made to
C                T_GFREPI during one GF search. Normally T_GFREPI
C                is called once per search pass.
C
C$ Exceptions
C
C     1) If NMAX is smaller than the declared length of the
C        static arrays used by this routine to store report 
C        logs, the error SPICE(BUG) will be signaled.
C
C     2) If this routine is called before the initialization
C        routine T_GFUINI is called, the behavior of this
C        system will be undefined.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine enables checking of the progress report confinement
C     windows and messages passed into the progress report
C     initialization routine T_GFREPI.
C
C$ Examples
C
C     See usage in F_GFDIRP.
C
C$ Restrictions
C
C     1) This routine relies on fixed dimension buffers. See the 
C        implementation and declarations in the umbrella routine
C        for details.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
C
C-&
 
C$ Index_Entries
C
C     return GF progress report initialization log
C
C-&

      CALL CHKIN ( 'T_GFRINI' )

C
C     Make sure we have room for the output.
C
      IF ( SVNINI .GT. NMAX ) THEN

         CALL SETMSG ( 'Number of progress reporting initialization '
     .   //            'calls (to T_GRREPI) since setup is #; room '
     .   //            'in output buffer is #.'                      )
         CALL ERRINT ( '#', SVNINI                                   )
         CALL ERRINT ( '#', NMAX                                     )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'T_GFRINI'                                    )
         RETURN

      END IF

C
C     Set the outputs:
C
C        - Number of calls to T_GFREPI
C        - Sequence numbers of the calls to T_GFREPI
C        - Input confinement windows
C        - Input messages
C

      NCALLS = SVNINI

      CALL MOVEI ( SVISEQ, SVNINI, SEQLOG )

      DO I = 1, SVNINI

         CALL COPYD ( SVCNFN(LBCELL,I), CNFLOG(LBCELL,I) )

         MSGLOG(1,I) = SVMSLG(1,I)
         MSGLOG(2,I) = SVMSLG(2,I)

      END DO

      CALL CHKOUT ( 'T_GFRINI' )
      RETURN





C$Procedure  T_GFREPU ( GF, GFREPU stand-in )
 
      ENTRY T_GFREPU ( IVBEG, IVEND, ET )

C$ Abstract
C
C     This is an instrumented replacement for GFREPU: this routine
C     logs its inputs so they may be examined after the completion
C     of a GF search.
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
C     GF
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      IVBEG
C     DOUBLE PRECISION      IVEND
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     IVBEG      I   Interval start time.
C     IVEND      I   Interval end time.
C     ET         I   Current time.
C
C$ Detailed_Input    
C 
C     IVBEG,
C     IVEND      are, respectively, the start and stop time of an
C                interval traversed during progress reporting.
C     
C     ET         is a time within the interval [IVBEG, IVEND].
C
C$ Detailed_Output
C
C     None. See Particulars.
C
C$ Parameters
C
C     MAXLOG     is the maximum number of calls that can be made to
C                this routine during one search. Normally this
C                routine is called multiple times per GF search pass.
C
C$ Exceptions
C
C     1) If more than MAXLOG calls are made to this routine
C        since the last call to T_GFUINI, the error SPICE(OVERFLOW)
C        is signaled.
C
C     2) If this routine is called before the initialization
C        routine T_GFUINI is called, the behavior of this
C        system will be undefined.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine should be used in place of GFREPU by test families
C     that examine progress reports created by mid-level GF search
C     routines.
C
C     Values stored by this routine may be retrieved by calling
C     
C        T_GFRPLO
C
C$ Examples
C
C     See usage in F_GFDIRP.
C
C$ Restrictions
C
C     1) This routine relies on fixed dimension buffers. See the 
C        implementation and declarations in the umbrella routine
C        for details.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
C
C-&
 
C$ Index_Entries
C
C     GF progress report update stand-in
C
C-&

      CALL CHKIN ( 'T_GFREPU' )

C
C     Increment the progress report update count.
C
      SVNUPD = SVNUPD + 1


      IF ( SVNUPD .GT. MAXLOG ) THEN

         CALL SETMSG ( 'Too many update calls: '
     .   //            'number of progress reporting update '
     .   //            'calls (to T_GRREPU) since setup is #; room '
     .   //            'in static T_GFTREP buffers is #.'            )
         CALL ERRINT ( '#', SVNUPD                                   )
         CALL ERRINT ( '#', MAXLOG                                   )
         CALL SIGERR ( 'SPICE(OVERFLOW)'                             )
         CALL CHKOUT ( 'T_GFREPU'                                    )
         RETURN

      END IF

C
C     Increment the call sequence index, and save this
C     index in the update sequence log.
C
      SVSQIX = SVSQIX + 1


      SVUSEQ ( SVNUPD ) = SVSQIX

C
C     Store the inputs in the update log.
C
      SVULOG ( 1, SVNUPD ) = IVBEG
      SVULOG ( 2, SVNUPD ) = IVEND
      SVULOG ( 3, SVNUPD ) = ET

      CALL CHKOUT ( 'T_GFREPU' )
      RETURN






C$Procedure  T_GFRPLO ( GF, return values passed to T_GFREPU )
 
      ENTRY T_GFRPLO ( NMAX, NCALLS, SEQLOG, REPLOG )

C$ Abstract
C
C
C     Return values passed to T_GFREPU, along with call sequence
C     indices.
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
C     GF
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     INTEGER               NMAX
C     INTEGER               NCALLS
C     INTEGER               SEQLOG ( * )
C     DOUBLE PRECISION      REPLOG ( 3, * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NMAX       I   Maximum number of log entries to return.
C     NCALLS     O   Number of T_GFREPI calls made during run.
C     SEQLOG     O   Log of T_GFREPI call sequence numbers.
C     REPLOG     O   Log of progress update intervals and times.
C
C$ Detailed_Input    
C 
C     NMAX       is the maximum number of log records that can be 
C                accommodated in the output arrays. NMAX should always
C                be set to a value at least equal to MAXLOG.
C
C$ Detailed_Output
C
C     NCALLS     is the number of calls to T_GFREPI made during a 
C                search. This count is reset to 0 every time the 
C                utility initialization entry point T_GFUINI is 
C                called.
C
C     SEQLOG     is a log of sequence numbers of the calls to
C                the routine T_GFREPU made during a test run. Sequence
C                numbers start at one and are monotone increasing.
C                Sequence numbers of the calls to to T_GFREPI,
C                T_GFREPU, and T_GFREPF form a consecutive sequence.
C                Normal GF progress reporting involves a sequence of
C                calls, for each search pass, having the pattern:
C
C                   T_GFREPI (once)
C                   T_GFREPU (multiple calls)
C                   T_GFREPF (once)
C
C     REPLOG    is a log of update intervals and times passed to
C               T_GFREPU. The Ith row of this array contains the
C               values passed to T_GFREPU on the Ith call since
C               the last utility initialization call. Within
C               the Ith row of this array, the consecutive elements
C               contain the values of IVBEG, IVEND, and ET passed
C               into T_GFPREP on the Ith call to T_GFREPU since
C               the last utility initialization call.
C
C
C$ Parameters
C
C     MAXLOG     is the maximum number of calls that can be made to
C                T_GFREPU during a search. Normally T_GFREPU is
C                called multiple times during one GF search pass.
C
C$ Exceptions
C
C     1) If NMAX is smaller than the declared length of the
C        static arrays used by this routine to store report 
C        logs, the error SPICE(BUG) will be signaled.
C
C     2) If this routine is called before the initialization
C        routine T_GFUINI is called, the behavior of this
C        system will be undefined.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine enables checking of the progress report confinement
C     windows and messages passed into the progress report
C     initialization routine T_GFREPU.
C
C$ Examples
C
C     See usage in F_GFDIRP.
C
C$ Restrictions
C
C     1) This routine relies on fixed dimension buffers. See the 
C        implementation and declarations in the umbrella routine
C        for details.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
C
C-&
 
C$ Index_Entries
C
C     return GF progress report update log
C
C-&


      CALL CHKIN ( 'T_GFRPLO' )

C
C     Make sure we have room for the output.
C
      IF ( SVNUPD .GT. NMAX ) THEN

         CALL SETMSG ( 'Number of progress reporting update '
     .   //            'calls (to T_GRREPU) since setup is #; room '
     .   //            'in buffer is #.'                             )
         CALL ERRINT ( '#', SVNUPD                                   )
         CALL ERRINT ( '#', NMAX                                     )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'T_GFRPLO'                                    )
         RETURN

      END IF

C
C     Set the outputs:
C
C        - Number of calls to T_GFREPU
C
C        - Sequence numbers of the calls to T_GFREPU
C
C        - Update log, which contains
C
C            > interval begin times
C            > interval end times
C            > ET values
C

      NCALLS = SVNUPD

      CALL MOVEI ( SVUSEQ, SVNUPD, SEQLOG )

      DO I = 1, SVNUPD

         REPLOG(1,I) = SVULOG(1,I)
         REPLOG(2,I) = SVULOG(2,I)
         REPLOG(3,I) = SVULOG(3,I)

      END DO

      CALL CHKOUT ( 'T_GFRPLO' )
      RETURN






C$Procedure  T_GFREPF ( GF, GFREPF stand-in )
 
      ENTRY T_GFREPF

C$ Abstract
C
C     This is an instrumented replacement for GFREPF: this routine logs
C     the sequence numbers of calls made to it so they may be examined
C     after the completion of a GF search.
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
C     GF
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     None.
C
C$ Detailed_Input    
C 
C     None.
C
C$ Detailed_Output
C
C     None. See Particulars.
C
C$ Parameters
C
C     MAXLOG     is the maximum number of calls that can be made to
C                this routine during one GF search.
C
C$ Exceptions
C
C     1) If more than MAXLOG calls are made to this routine
C        since the last call to T_GFUINI, the error SPICE(OVERFLOW)
C        is signaled.
C
C     2) If this routine is called before the initialization
C        routine T_GFUINI is called, the behavior of this
C        system will be undefined.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine should be used in place of GFREPF by test families
C     that examine progress reports created by mid-level GF search
C     routines.
C
C     Values stored by this routine may be retrieved by calling
C     
C        T_GFRTRM
C
C$ Examples
C
C     See usage in F_GFDIRP.
C
C$ Restrictions
C
C     1) This routine relies on fixed dimension buffers. See the 
C        implementation and declarations in the umbrella routine
C        for details.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
C
C-&
 
C$ Index_Entries
C
C     GF progress report termination stand-in
C
C-&

      CALL CHKIN ( 'T_GFREPF' )

C
C     Increment the progress report termination count.
C
      SVNFIN = SVNFIN + 1


      IF ( SVNFIN .GT. MAXLOG ) THEN

         CALL SETMSG ( 'Too many termination calls: '
     .   //            'number of progress reporting termination '
     .   //            'calls (to T_GRREPF) since setup is #; room '
     .   //            'in static T_GFTREP buffers is #.'            )
         CALL ERRINT ( '#', SVNFIN                                   )
         CALL ERRINT ( '#', MAXLOG                                   )
         CALL SIGERR ( 'SPICE(OVERFLOW)'                             )
         CALL CHKOUT ( 'T_GFREPF'                                    )
         RETURN

      END IF


C
C     Increment the call sequence index, and save this
C     index in the termination sequence log.
C
      SVSQIX            = SVSQIX + 1
      SVFSEQ ( SVNFIN ) = SVSQIX

      CALL CHKOUT ( 'T_GFREPF' )
      RETURN







C$Procedure  T_GFRTRM ( GF, return values passed to T_GFREPU )
 
      ENTRY T_GFRTRM ( NMAX, NCALLS, SEQLOG )

C$ Abstract
C
C     Return call sequence indices for T_GFREPF calls.
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
C     GF
C     TIME
C
C$ Keywords
C
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     INTEGER               NMAX
C     INTEGER               NCALLS
C     INTEGER               SEQLOG ( * )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NMAX       I   Maximum number of log entries to return.
C     NCALLS     O   Number of T_GFREPF calls made during run.
C     SEQLOG     O   Log of T_GFREPF call sequence numbers.
C
C$ Detailed_Input    
C 
C     NMAX       is the maximum number of log records that can be 
C                accommodated in the output arrays. NMAX should always
C                be set to a value at least equal to MXPASS.
C
C$ Detailed_Output
C
C     NCALLS     is the number of calls to T_GFREPF made during a 
C                search. This count is reset to 0 every time the 
C                utility initialization entry point T_GFUINI is 
C                called.
C
C     SEQLOG     is a log of sequence numbers of the calls to
C                the routine T_GFREPF made during a test run. Sequence
C                numbers start at one and are monotone increasing.
C                Sequence numbers of the calls to to T_GFREPI,
C                T_GFREPU, and T_GFREPF form a consecutive sequence.
C                Normal GF progress reporting involves a sequence of
C                calls, for each search pass, having the pattern:
C
C                   T_GFREPI (once)
C                   T_GFREPU (multiple calls)
C                   T_GFREPF (once)
C
C
C$ Parameters
C
C     MXPASS     is the maximum number of calls that can be made to
C                T_GFREPF during a search. Normally T_GFREPF is
C                called once per GF search pass.
C
C$ Exceptions
C
C     1) If NMAX is smaller than the declared length of the
C        static arrays used by this routine to store report 
C        logs, the error SPICE(BUG) will be signaled.
C
C     2) If this routine is called before the initialization
C        routine T_GFUINI is called, the behavior of this
C        system will be undefined.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine enables checking of the sequence of calls made to
C     the progress report termination routine T_GFREPF.
C
C$ Examples
C
C     See usage in F_GFDIRP.
C
C$ Restrictions
C
C     1) This routine relies on fixed dimension buffers. See the 
C        implementation and declarations in the umbrella routine
C        for details.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.0, 23-FEB-2009 (NJB) (EDW)
C
C-&
 
C$ Index_Entries
C
C     return GF progress report termination log
C
C-&
 

      CALL CHKIN ( 'T_GFRTRM' )

C
C     Make sure we have room for the output.
C
      IF ( SVNFIN .GT. NMAX ) THEN

         CALL SETMSG ( 'Number of progress reporting termination '
     .   //            'calls (to T_GRREPF) since setup is #; room '
     .   //            'in buffer is #.'                             )
         CALL ERRINT ( '#', SVNFIN                                   )
         CALL ERRINT ( '#', NMAX                                     )
         CALL SIGERR ( 'SPICE(BUG)'                                  )
         CALL CHKOUT ( 'T_GFRTRM'                                    )
         RETURN

      END IF

C
C     Set the outputs:
C
C        - Number of calls to T_GFRFIN
C
C        - Sequence numbers of the calls to T_GFRFIN
C

      NCALLS = SVNFIN

      CALL MOVEI ( SVFSEQ, SVNFIN, SEQLOG )

      CALL CHKOUT ( 'T_GFRTRM' )
      RETURN
      END






      
