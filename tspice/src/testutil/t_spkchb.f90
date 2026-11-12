!$Procedure T_SPKCHB ( Test utility, fit SPK data with Chebys )

      SUBROUTINE T_SPKCHB ( TARGET, CENTER, FRAME, FIRST,  LAST, &
                            NPOS,   NVEL,   WORK,  POSCOF, VELCOF )
      use pepper_legacy, pepper_legacy_frame => frame

! $ Abstract
!
!     Create Chebyshev expansions for the position and velocity
!     of an object whose ephemeris is given by SPK data.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     CHEBYSHEV
!     FIT
!     POLYNOMIAL
!     SPK
!     TEST
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               MAXSIZ
      PARAMETER           ( MAXSIZ = 25 )

      CHARACTER*(*)         TARGET
      CHARACTER*(*)         CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      INTEGER               NPOS
      INTEGER               NVEL
      DOUBLE PRECISION      WORK   ( * )
      DOUBLE PRECISION      POSCOF ( NPOS, 3 )
      DOUBLE PRECISION      VELCOF ( NVEL, 3 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     MAXSIZ     P   Maximum number of terms in expansion.
!     TARGET     I   Target body name.
!     CENTER     I   Center name.
!     FRAME      I   Frame name.
!     FIRST      I   Interval start time in seconds past J2000 TDB.
!     LAST       I   Interval stop time in seconds past J2000 TDB.
!     NPOS       I   Number of terms in position expansion.
!     NVEL       I   Number of terms in velocity expansion.
!     WORK      I-O  Workspace.
!     POSCOF     O   Chebyshev coefficients for position.
!     VELCOF     O   Chebyshev coefficients for velocity.
!
! $ Detailed_Input
!
!     TARGET         is the name of the target body. Chebyshev
!                    coefficients for the state of the target relative
!                    to the center, in the reference frame specified by
!                    FRAME, are computed by this routine.
!
!     CENTER         is the name of the center of motion.
!
!     FRAME          is the name of the reference frame in which to
!                    states are to be computed.
!
!     FIRST,
!     LAST           are the time bounds of the time interval over
!                    which the Chebyshev expansion is applicable. Times
!                    are expressed as seconds past J2000 TDB.
!
!     NPOS           is the number of terms in the position expansion.
!                    The degree of the expansion is NPOS-1. The maximum
!                    value of NPOS is limited by the parameter MAXSIZ.
!
!     NVEL           is the number of terms in the velocity expansion.
!                    The degree of the expansion is NVEL-1. The maximum
!                    value of NVEL is limited by the parameter MAXSIZ.
!
!     WORK           is a workspace array; this workspace is used by
!                    the Chebyshev fitter CHBFIT. The size of WORK must
!                    be at least
!
!                       MAX ( NPOS, NVEL )  +  1
!
! $ Detailed_Output
!
!     WORK           is the workspace array, modified by this
!                    routine.
!
!     POSCOF         is a Chebyshev expansion of degree NPOS-1 for the
!                    position of TARGET with respect to CENTER, in the
!                    reference frame FRAME, over the time interval
!                    [FIRST, LAST].
!
!
!     VELCOF         is the corresponding Chebyshev expansion
!                    for velocity. The degree is NVEL-1.
!
! $ Parameters
!
!     MAXSIZ is the maximum number of terms in either output
!     expansion. This parameter is declared in the support
!     library utility routine CHBFIT.
!
! $ Exceptions
!
!     1) If either the input target or center name cannot be mapped
!        to an ID code, the error SPICE(NOTRANSLATION) is signaled.
!
!     2) If either NPOS or NVEL is outside of the range 1:MAXSIZ,
!        the error SPICE(INVALIDCOUNT) is signaled.
!
!     3) Any exceptions that may be incurred by SPKEZ may be incurred
!        by this routine.
!
! $ Files
!
!     1) SPK data are required to provide the state of the target
!        relative to the center of motion over the time interval
!        [FIRST, LAST].
!
! $ Particulars
!
!     This routine supports testing of Chebyshev-based SPK and PCK
!     code.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) The quality of the Chebyshev expansions produced by
!        this routine is dependent on the suitability of the
!        input data. Attempting to fit ephemerides over
!        excessively long time intervals will result in
!        useless outputs.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 26-NOV-2013 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Other functions
!
      DOUBLE PRECISION      T_GETX
      DOUBLE PRECISION      T_GETY
      DOUBLE PRECISION      T_GETZ
      DOUBLE PRECISION      T_GETDX
      DOUBLE PRECISION      T_GETDY
      DOUBLE PRECISION      T_GETDZ
      DOUBLE PRECISION      T_STCINI

      EXTERNAL T_GETX
      EXTERNAL T_GETY
      EXTERNAL T_GETZ
      EXTERNAL T_GETDX
      EXTERNAL T_GETDY
      EXTERNAL T_GETDZ

!
!     Local parameters
!

!
!     Local variables
!
      DOUBLE PRECISION      RETVAL

      INTEGER               CENTID
      INTEGER               TARGID

      LOGICAL               FOUND

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_SPKCHB' )


      CALL BODS2C ( TARGET, TARGID, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Target name # could not be translated to ' &
         //            'an ID code.'                              )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                     )
         CALL CHKOUT ( 'T_SPKCHB'                                 )
         RETURN

      END IF

      CALL BODS2C ( CENTER, CENTID, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Center name # could not be translated to ' &
         //            'an ID code.'                              )
         CALL ERRCH  ( '#', CENTER                                )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                     )
         CALL CHKOUT ( 'T_SPKCHB'                                 )
         RETURN

      END IF

      IF ( ( NPOS .LT. 1 ) .OR. ( NPOS .GT. MAXSIZ )  ) THEN

         CALL SETMSG ( 'Position coefficient count # must be in ' &
         //            'the range 1:# but was #.'                 )
         CALL ERRINT ( '#', MAXSIZ                                )
         CALL ERRINT ( '#', NPOS                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                      )
         CALL CHKOUT ( 'T_SPKCHB'                                 )
         RETURN

      END IF

      IF ( ( NVEL .LT. 1 ) .OR. ( NVEL .GT. MAXSIZ )  ) THEN

         CALL SETMSG ( 'Velocity coefficient count # must be in ' &
         //            'the range 1:# but was #.'                 )
         CALL ERRINT ( '#', MAXSIZ                                )
         CALL ERRINT ( '#', NVEL                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                      )
         CALL CHKOUT ( 'T_SPKCHB'                                 )
         RETURN

      END IF
!
!     Initialize the state component functions. The returned
!     value is not used; the called function operates by
!     side effects.
!
      RETVAL = T_STCINI ( TARGID, FRAME, 'NONE', CENTID )
!
!     Fit the state vector components.
!
      CALL CHBFIT ( T_GETX,  FIRST, LAST, NPOS, WORK, POSCOF(1,1) )
      CALL CHBFIT ( T_GETY,  FIRST, LAST, NPOS, WORK, POSCOF(1,2) )
      CALL CHBFIT ( T_GETZ,  FIRST, LAST, NPOS, WORK, POSCOF(1,3) )
      CALL CHBFIT ( T_GETDX, FIRST, LAST, NVEL, WORK, VELCOF(1,1) )
      CALL CHBFIT ( T_GETDY, FIRST, LAST, NVEL, WORK, VELCOF(1,2) )
      CALL CHBFIT ( T_GETDZ, FIRST, LAST, NVEL, WORK, VELCOF(1,3) )

      CALL CHKOUT ( 'T_SPKCHB' )
      RETURN
      END


