C$Procedure T_TSTRLN (Test RESLUN)
 
      SUBROUTINE T_TSTRLN ( LUN, RESRVD )
 
C$ Abstract
C
C     Test whether LUN has been reserved.
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
 
      INTEGER               LUN
      LOGICAL               RESRVD
 
      INTEGER               MAXLUN
      PARAMETER           ( MAXLUN = 200 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LUN        I   Logical unit in question
C     RESRVD     O   Logical indicating if LUN appears to be reserved.
C     MAXLUN     P   Maximum number of logical units to buffer.
C
C$ Detailed_Input
C
C     LUN        is a FORTRAN logical unit returned by GETLUN or
C                FNDLUN.
C
C$ Detailed_Output
C
C     RESRVD     is a logical that if set to TRUE indicates that all
C                logical units returned by FNDLUN did not contain LUN.
C                This is the best we can do.  If FALSE then LUN was
C                returned by FNDLUN.
C
C$ Parameters
C
C     MAXLUN     is a parameter whose value must exceed the maximum
C                number of distinct logical units that FNDLUN may
C                return for any supported platform.
C
C$ Exceptions
C
C     1) SPICE(INQUIREFAILED) is signaled if the INQUIRE on LUN to
C        determine if a file is attached fails.
C
C     2) SPICE(LUNINUSE) is signaled if the INQUIRE on LUN indicates
C        a file is attached to LUN.
C
C     3) SPICE(FILEOPENFAILED) is signaled if an IOSTAT error
C        occurs when trying to attach a unit returned from FNDLUN
C        to a file.
C
C     4) If the parameter MAXLUN is not greater than the number
C        of distinct logical units that FNDLUN returns on this
C        platform, then the error SPICE(PARAMTOOSMALL) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine checks to see if a particular logical unit has been
C     reserved with RESLUN.
C
C$ Examples
C
C     See F_DDHRMU for sample usage.
C
C$ Restrictions
C
C     A file may not be attached to LUN when this routine is
C     invoked.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C     E.D. Wright     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    TSPICE Version 1.3.0, 28-NOV-2021 (BVS)
C
C        Updated for MAC-OSX-M1-64BIT-CLANG_C.
C
C-    TSPICE Version 1.2.0, 09-APR-2014 (BVS)
C
C        Bug fix: replaced with TXTOPN with OPEN. Made this routine
C        a master file.
C
C-    TSPICE Version 1.1.0, 01-NOV-2009 (EDW)
C
C        Eliminated use of FORTRAN SCRATCH files due to mktemp
C        behavior in MS VS 2008. This routine now creates and
C        manages its temporary files.
C
C        Reordered header sections to NAIF spec.
C
C        Note: Do we need these tests?
C
C-    TSPICE Version 1.0.0, 27-JUN-2001 (FST)
C
C-&
 
C
C     SPICELIB Functions
C
      LOGICAL               RETURN
      LOGICAL               FAILED
 
C
C     Local Variables
C
      INTEGER               LUNARY ( MAXLUN )
      INTEGER               IOSTAT
      INTEGER               I
      INTEGER               J
 
      LOGICAL               DONE
      LOGICAL               OPENED
      LOGICAL               LUNFND
 
      CHARACTER*(128)       FNAME
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_TSTRLN' )
      END IF
 
C
C     Assume LUN is not reserved until proven otherwise.
C
      RESRVD = .FALSE.
 
C
C     INQUIRE on LUN to see if it is attached to a file.
C
      INQUIRE ( UNIT   = LUN,
     .          OPENED = OPENED,
     .          IOSTAT = IOSTAT  )
 
C
C     Check IOSTAT for troubles.
C
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Attempt to perform INQUIRE on logical unit, '
     .   //            '''#'', failed.  IOSTAT = #.'                  )
         CALL ERRINT ( '#', LUN                                       )
         CALL ERRINT ( '#', IOSTAT                                    )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                         )
         CALL CHKOUT ( 'T_TSTRLN'                                     )
         RETURN
      END IF
 
C
C     Now check to see if LUN is attached to a file.
C
      IF ( OPENED ) THEN
         CALL SETMSG ( 'The logical unit, ''#'', is currently '
     .   //            'attached to a file.  This routine can '
     .   //            'only verify the reserved state of units '
     .   //            'not currently attached to files.'         )
         CALL ERRINT ( '#', LUN                                   )
         CALL SIGERR ( 'SPICE(LUNINUSE)'                          )
         CALL CHKOUT ( 'T_TSTRLN'                                 )
         RETURN
      END IF
 
C
C     This is a total kludge, but it will work.  We are going to
C     call FNDLUN until we run out of space for storing units or
C     we have exhausted all possible logical units.
C
      LUNFND = .FALSE.
      DONE   = .FALSE.
      I      = 0
 
      DO WHILE ( .NOT. DONE )
 
C
C        Increment I.
C
         I = I + 1
 
C
C        Fetch the next unit from FNDLUN.
C
         CALL FNDLUN ( LUNARY(I) )
 
C
C        Check to see if the INQUIRE buried in FNDLUN failed.
C
         IF ( LUNARY(I) .LT. 0 ) THEN
            CALL SETMSG ( 'INQUIRE failed. IOSTAT = #.' )
            CALL ERRINT ( '#', - LUNARY(I)              )
            CALL SIGERR ( 'SPICE(INQUIREFAILED)'        )
            CALL CHKOUT ( 'T_TSTRLN'                    )
            RETURN
         END IF
 
C
C        Now see if LUN is LUNARY(I).  If it is stop, because
C        clearly LUN isn't reserved.
C
         IF ( LUN .EQ. LUNARY(I) ) THEN
 
            DONE   = .TRUE.
            LUNFND = .TRUE.
 
C
C        Then check to see if LUNARY(I) is 0, indicating that
C        FNDLUN was unable to locate a new logical unit, in
C        which case, we are finished.
C
         ELSE IF ( LUNARY(I) .EQ. 0 ) THEN
 
            DONE = .TRUE.
 
C
C        Otherwise open a scratch file on LUNARY(I) so FNDLUN
C        won't return it again.
C
         ELSE
 
C
C           Create a temporary, unused file, at logical unit
C           LUNARY(I).
C
            FNAME = 'tmp.#'
            CALL REPMI  ( FNAME, '#', I, FNAME )
 
            OPEN ( UNIT            =  LUNARY(I),
     .             FILE            =  FNAME,
     .             FORM            = 'FORMATTED',
     .             ACCESS          = 'SEQUENTIAL',
     .             STATUS          = 'NEW',
     .             IOSTAT          =  IOSTAT      )
 
            IF ( IOSTAT .NE. 0 ) THEN
               CALL SETMSG ( 'Could not open file #. IOSTAT was #. ' )
               CALL ERRCH  ( '#', FNAME                              )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                 )
               CALL CHKOUT ( 'T_TSTRLN'                              )
               RETURN
            END IF
 
         END IF
 
C
C        Now see if entering the loop again will push I past
C        MAXLUN.  This should never happen since MAXLUN is
C        greater than the number of units for any platform.
C
         IF ( ( I .EQ. MAXLUN ) .OR. ( LUNARY(I) .EQ. 0 ) ) THEN
            DONE = .TRUE.
         END IF
 
 
      END DO
 
C
C     Wrap up our analysis. Clean up the open units and delete the
C     corresponding files.
C
      DO J = 1, I-1
 
         CLOSE ( UNIT = LUNARY(J) )
 
         FNAME = 'tmp.#'
         CALL REPMI  ( FNAME, '#', J, FNAME )
         CALL DELFIL ( FNAME )
 
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_TSTRLN' )
            RETURN
         END IF
 
      END DO
 
C
C     If we didn't find LUN in the list, then see if we exhausted
C     all possible logical units.
C
      IF ( .NOT. LUNFND ) THEN
 
C
C        First check to see if we exhausted the available space to
C        store units.  If we did, signal an error to warn the
C        caller that the test results are inconclusive.
C
         IF ( ( I .EQ. MAXLUN ) .AND. ( LUNARY(I) .GT. 0 ) ) THEN
 
            CLOSE ( UNIT = LUNARY(I) )
            CALL SETMSG ( 'FNDLUN returned too many logical units '
     .      //            'for the test to be conclusive.  Increase '
     .      //            'the parameter MAXLUN and recompile this '
     .      //            'module.'                                   )
            CALL SIGERR ( 'SPICE(PARAMTOOSMALL)'                      )
            CALL CHKOUT ( 'T_TSTRLN'                                  )
            RETURN
 
         END IF
 
C
C        If we make it this far, we did not use all of the available
C        space to store units.  Just for safety, check to see that
C        LUNARY(I) is 0.
C
         IF ( LUNARY(I) .NE. 0 ) THEN
 
            CLOSE ( UNIT = LUNARY(I) )
C
C           This should never happen, signal SPICE(BUG) and return.
C
            CALL SETMSG ( 'All of the unit storage space was not '
     .      //            'exhausted, and the last logical unit '
     .      //            'returned by FNDLUN was not zero.  This '
     .      //            'should never happen.'                    )
            CALL SIGERR ( 'SPICE(BUG)'                              )
            CALL CHKOUT ( 'T_TSTRLN'                                )
            RETURN
 
         END IF
 
C
C        All that remains is to set RESRVD to TRUE, since we have
C        exhausted all of the units in FNDLUN and LUN did not turn
C        up.
C
         RESRVD = .TRUE.
 
      END IF
 
      CALL CHKOUT ( 'T_TSTRLN' )
      RETURN
 
      END
