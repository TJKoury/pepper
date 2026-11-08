!$Procedure GFSTEP ( GF, step size )

      SUBROUTINE GFSTEP ( TIME, STEP )

! $ Abstract
!
!     Return the time step set by the most recent call to GFSSTP.
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

      DOUBLE PRECISION      TIME
      DOUBLE PRECISION      STEP

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TIME       I   Ignored ET value.
!     STEP       O   Time step to take.
!
! $ Detailed_Input
!
!     TIME     is an ignored double precision number. This argument
!              is present so the argument list of this routine is
!              compatible with the GF step size routine argument list
!              specification.
!
!              When this routine is called from within the GF
!              root-finding system, either the initial ET value of the
!              current interval of the confinement window, or the
!              value resulting from the last search step, is passed in
!              via the TIME argument.
!
! $ Detailed_Output
!
!     STEP     is the output step size. This is the value set by the
!              most recent call to GFSSTP. Units are TDB seconds.
!
!              STEP is used in the GF search root-bracketing process.
!              STEP indicates how far to advance TIME so that TIME and
!              TIME+STEP may bracket a state transition and definitely
!              do not bracket more than one state transition.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called before a step size has been
!         set via a call to GFSSTP, the error SPICE(NOTINITIALIZED)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine returns the time step set by the most recent call to
!     GFSSTP.
!
! $ Examples
!
!     1) In normal usage of a high-level GF API routine, the caller
!        will pass in a constant step size STEP. The API routine will
!        then make the call
!
!           CALL GFSSTP ( STEP )
!
!        Subsequent calls to GFSTEP during the search process conducted
!        by the API routine will return STEP.
!
!
!     2) User applications can pass GFSTEP to mid-level GF API routines
!        expecting a step size routine as an input argument. For
!        example, the GF API routine GFOCCE can be called as follows:
!
!
!           Set the step size.
!
!           CALL GFSSTP ( STEP )
!
!
!           Look for solutions. (GFSTEP is the 11th argument.)
!
!           CALL GFOCCE ( OCCTYP,  FRONT,   FSHAPE,  FFRAME,
!          .              BACK,    BSHAPE,  BFRAME,  ABCORR,
!          .              OBSRVR,  CNVTOL,  GFSTEP,  GFREFN,
!          .              RPT,     GFREPI,  GFREPU,  GFREPF,
!          .              BAIL,    GFBAIL,  CNFINE,  RESULT )
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
!     L.S. Elson         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 31-AUG-2010 (EDW)
!
!        Expanded error message on STEP for clarity.
!
!        Added TIME = TIME declaration to eliminate unused dummy
!        variable warning during compilation.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (IMU) (WLT) (EDW)
!
!-&


! $ Index_Entries
!
!     GF get constant step size
!
!-&


!
!     SPICELIB functions
!

!
!     Local variables
!
      DOUBLE PRECISION      SVSTEP

      LOGICAL               SVINIT

!
!     Saved variables
!
      SAVE                  SVSTEP
      SAVE                  SVINIT

!
!     Initial values
!
      DATA                  SVINIT / .FALSE. /
      DATA                  SVSTEP / -1.0D0  /


!
!     Discovery check-in.
!
      IF ( .NOT. SVINIT ) THEN

         CALL CHKIN  ( 'GFSTEP'                           )
         CALL SETMSG ( 'Step size was never initialized.' )
         CALL SIGERR ( 'SPICE(NOTINITIALIZED)'            )
         CALL CHKOUT ( 'GFSTEP'                           )
         RETURN

      END IF

!
!     Set STEP to the saved value from the last call to GFSSTP.
!
      STEP = SVSTEP
      TIME = TIME

      RETURN



!$Procedure GFSSTP ( Geometry finder set step size )

      ENTRY GFSSTP ( STEP )

! $ Abstract
!
!     Set the step size to be returned by GFSTEP.
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
!     DOUBLE PRECISION      STEP
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STEP       I   Time step to take.
!
! $ Detailed_Input
!
!     STEP     is the output step size to be returned by the next call
!              GFSTEP. Units are TDB seconds.
!
!              STEP is used in the GF search root-bracketing process.
!              STEP indicates how far to advance TIME so that TIME and
!              TIME+STEP may bracket a state transition and definitely
!              do not bracket more than one state transition.
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
!     1)  If the input step size is non-positive, the error
!         SPICE(INVALIDSTEP) is signaled. The stored step value
!         is not updated.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See the header of GFSTEP above.
!
! $ Examples
!
!     See the header of GFSTEP above.
!
! $ Restrictions
!
!     1)  This routine must be called before the first time
!         GFSTEP is called during a program run.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     L.S. Elson         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 31-AUG-2010 (EDW)
!
!        Expanded error message on STEP for clarity.
!
!-    SPICELIB Version 1.0.0, 15-APR-2009 (LSE) (NJB)
!
!-&


! $ Index_Entries
!
!     GF set constant step size
!
!-&


!
!     Check the step size.
!
      IF ( STEP .LE. 0.D0 ) THEN

         CALL CHKIN  ( 'GFSSTP'                                       )
         CALL SETMSG ( 'Step has value #; step size must be positive.')
         CALL ERRDP  ( '#',  STEP                                     )
         CALL SIGERR ( 'SPICE(INVALIDSTEP)'                           )
         CALL CHKOUT ( 'GFSSTP'                                       )
         RETURN

      END IF

      SVSTEP = STEP
      SVINIT = .TRUE.

      RETURN
      END
