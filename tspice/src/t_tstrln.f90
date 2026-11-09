!$Procedure T_TSTRLN (Test RESLUN)

      SUBROUTINE T_TSTRLN ( LUN, RESRVD )

! $ Abstract
!
!     Test whether LUN has been reserved.
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

      INTEGER               LUN
      LOGICAL               RESRVD

      INTEGER               MAXLUN
      PARAMETER           ( MAXLUN = 200 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LUN        I   Logical unit in question
!     RESRVD     O   Logical indicating if LUN appears to be reserved.
!     MAXLUN     P   Maximum number of logical units to buffer.
!
! $ Detailed_Input
!
!     LUN        is a FORTRAN logical unit returned by GETLUN or
!                FNDLUN.
!
! $ Detailed_Output
!
!     RESRVD     is a logical that if set to TRUE indicates that all
!                logical units returned by FNDLUN did not contain LUN.
!                This is the best we can do.  If FALSE then LUN was
!                returned by FNDLUN.
!
! $ Parameters
!
!     MAXLUN     is a parameter whose value must exceed the maximum
!                number of distinct logical units that FNDLUN may
!                return for any supported platform.
!
! $ Exceptions
!
!     1) SPICE(INQUIREFAILED) is signaled if the INQUIRE on LUN to
!        determine if a file is attached fails.
!
!     2) SPICE(LUNINUSE) is signaled if the INQUIRE on LUN indicates
!        a file is attached to LUN.
!
!     3) SPICE(FILEOPENFAILED) is signaled if an IOSTAT error
!        occurs when trying to attach a unit returned from FNDLUN
!        to a file.
!
!     4) If the parameter MAXLUN is not greater than the number
!        of distinct logical units that FNDLUN returns on this
!        platform, then the error SPICE(PARAMTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine checks to see if a particular logical unit has been
!     reserved with RESLUN.
!
! $ Examples
!
!     See F_DDHRMU for sample usage.
!
! $ Restrictions
!
!     A file may not be attached to LUN when this routine is
!     invoked.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!     E.D. Wright     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    TSPICE Version 1.3.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    TSPICE Version 1.2.0, 09-APR-2014 (BVS)
!
!        Bug fix: replaced with TXTOPN with OPEN. Made this routine
!        a master file.
!
!-    TSPICE Version 1.1.0, 01-NOV-2009 (EDW)
!
!        Eliminated use of FORTRAN SCRATCH files due to mktemp
!        behavior in MS VS 2008. This routine now creates and
!        manages its temporary files.
!
!        Reordered header sections to NAIF spec.
!
!        Note: Do we need these tests?
!
!-    TSPICE Version 1.0.0, 27-JUN-2001 (FST)
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               RETURN
      LOGICAL               FAILED

!
!     Local Variables
!
      INTEGER               LUNARY ( MAXLUN )
      INTEGER               IOSTAT
      INTEGER               I
      INTEGER               J

      LOGICAL               DONE
      LOGICAL               OPENED
      LOGICAL               LUNFND

      CHARACTER*(128)       FNAME


!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_TSTRLN' )
      END IF

!
!     Assume LUN is not reserved until proven otherwise.
!
      RESRVD = .FALSE.

!
!     INQUIRE on LUN to see if it is attached to a file.
!
      INQUIRE ( UNIT   = LUN, &
                OPENED = OPENED, &
                IOSTAT = IOSTAT  )

!
!     Check IOSTAT for troubles.
!
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Attempt to perform INQUIRE on logical unit, ' &
         //            '''#'', failed.  IOSTAT = #.'                  )
         CALL ERRINT ( '#', LUN                                       )
         CALL ERRINT ( '#', IOSTAT                                    )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                         )
         CALL CHKOUT ( 'T_TSTRLN'                                     )
         RETURN
      END IF

!
!     Now check to see if LUN is attached to a file.
!
      IF ( OPENED ) THEN
         CALL SETMSG ( 'The logical unit, ''#'', is currently ' &
         //            'attached to a file.  This routine can ' &
         //            'only verify the reserved state of units ' &
         //            'not currently attached to files.'         )
         CALL ERRINT ( '#', LUN                                   )
         CALL SIGERR ( 'SPICE(LUNINUSE)'                          )
         CALL CHKOUT ( 'T_TSTRLN'                                 )
         RETURN
      END IF

!
!     This is a total kludge, but it will work.  We are going to
!     call FNDLUN until we run out of space for storing units or
!     we have exhausted all possible logical units.
!
      LUNFND = .FALSE.
      DONE   = .FALSE.
      I      = 0

      DO WHILE ( .NOT. DONE )

!
!        Increment I.
!
         I = I + 1

!
!        Fetch the next unit from FNDLUN.
!
         CALL FNDLUN ( LUNARY(I) )

!
!        Check to see if the INQUIRE buried in FNDLUN failed.
!
         IF ( LUNARY(I) .LT. 0 ) THEN
            CALL SETMSG ( 'INQUIRE failed. IOSTAT = #.' )
            CALL ERRINT ( '#', - LUNARY(I)              )
            CALL SIGERR ( 'SPICE(INQUIREFAILED)'        )
            CALL CHKOUT ( 'T_TSTRLN'                    )
            RETURN
         END IF

!
!        Now see if LUN is LUNARY(I).  If it is stop, because
!        clearly LUN isn't reserved.
!
         IF ( LUN .EQ. LUNARY(I) ) THEN

            DONE   = .TRUE.
            LUNFND = .TRUE.

!
!        Then check to see if LUNARY(I) is 0, indicating that
!        FNDLUN was unable to locate a new logical unit, in
!        which case, we are finished.
!
         ELSE IF ( LUNARY(I) .EQ. 0 ) THEN

            DONE = .TRUE.

!
!        Otherwise open a scratch file on LUNARY(I) so FNDLUN
!        won't return it again.
!
         ELSE

!
!           Create a temporary, unused file, at logical unit
!           LUNARY(I).
!
            FNAME = 'tmp.#'
            CALL REPMI  ( FNAME, '#', I, FNAME )

            OPEN ( UNIT            =  LUNARY(I), &
                   FILE            =  FNAME, &
                   FORM            = 'FORMATTED', &
                   ACCESS          = 'SEQUENTIAL', &
                   STATUS          = 'NEW', &
                   IOSTAT          =  IOSTAT      )

            IF ( IOSTAT .NE. 0 ) THEN
               CALL SETMSG ( 'Could not open file #. IOSTAT was #. ' )
               CALL ERRCH  ( '#', FNAME                              )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                 )
               CALL CHKOUT ( 'T_TSTRLN'                              )
               RETURN
            END IF

         END IF

!
!        Now see if entering the loop again will push I past
!        MAXLUN.  This should never happen since MAXLUN is
!        greater than the number of units for any platform.
!
         IF ( ( I .EQ. MAXLUN ) .OR. ( LUNARY(I) .EQ. 0 ) ) THEN
            DONE = .TRUE.
         END IF


      END DO

!
!     Wrap up our analysis. Clean up the open units and delete the
!     corresponding files.
!
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

!
!     If we didn't find LUN in the list, then see if we exhausted
!     all possible logical units.
!
      IF ( .NOT. LUNFND ) THEN

!
!        First check to see if we exhausted the available space to
!        store units.  If we did, signal an error to warn the
!        caller that the test results are inconclusive.
!
         IF ( ( I .EQ. MAXLUN ) .AND. ( LUNARY(I) .GT. 0 ) ) THEN

            CLOSE ( UNIT = LUNARY(I) )
            CALL SETMSG ( 'FNDLUN returned too many logical units ' &
            //            'for the test to be conclusive.  Increase ' &
            //            'the parameter MAXLUN and recompile this ' &
            //            'module.'                                   )
            CALL SIGERR ( 'SPICE(PARAMTOOSMALL)'                      )
            CALL CHKOUT ( 'T_TSTRLN'                                  )
            RETURN

         END IF

!
!        If we make it this far, we did not use all of the available
!        space to store units.  Just for safety, check to see that
!        LUNARY(I) is 0.
!
         IF ( LUNARY(I) .NE. 0 ) THEN

            CLOSE ( UNIT = LUNARY(I) )
!
!           This should never happen, signal SPICE(BUG) and return.
!
            CALL SETMSG ( 'All of the unit storage space was not ' &
            //            'exhausted, and the last logical unit ' &
            //            'returned by FNDLUN was not zero.  This ' &
            //            'should never happen.'                    )
            CALL SIGERR ( 'SPICE(BUG)'                              )
            CALL CHKOUT ( 'T_TSTRLN'                                )
            RETURN

         END IF

!
!        All that remains is to set RESRVD to TRUE, since we have
!        exhausted all of the units in FNDLUN and LUN did not turn
!        up.
!
         RESRVD = .TRUE.

      END IF

      CALL CHKOUT ( 'T_TSTRLN' )
      RETURN

      END
