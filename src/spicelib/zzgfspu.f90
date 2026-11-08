!$Procedure ZZGFSPU ( Private - GF, angular separation routines )

      SUBROUTINE ZZGFSPU ( OF,     FROM,   SHAPE,  FRAME,  ET, &
                           UDFUNC, ABCORR, DECRES, SEP, &
                           XABCR,  XBOD,   YREF,   XREF, &
                           XOBS,   XRAD,   XSHP )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the umbrella routine for the entry points needed by
!     GFEVNT in order to find angular separation events.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     ROOT
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         OF    (2)
      CHARACTER*(*)         FROM
      CHARACTER*(*)         SHAPE (2)
      CHARACTER*(*)         FRAME (2)
      DOUBLE PRECISION      ET
      EXTERNAL              UDFUNC
      CHARACTER*(*)         ABCORR
      LOGICAL               DECRES
      DOUBLE PRECISION      SEP
      CHARACTER*(*)         XABCR
      INTEGER               XBOD  (2)
      CHARACTER*(*)         YREF
      CHARACTER*(*)         XREF  (2)
      INTEGER               XOBS
      DOUBLE PRECISION      XRAD  (2)
      INTEGER               XSHP  (2)

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     OF         I   ZZGFSPIN
!     FROM       I   ZZGFSPIN
!     SHAPE      I   ZZGFSPIN
!     FRAME      I   ZZGFSPIN
!     ET         I   ZZGFSPDC, ZZGFSPGQ
!     UDFUNC     I   ZZGFSPDC
!     ABCORR     I   ZZGFSPIN
!     DECRES     O   ZZGFSPDC
!     SEP        O   ZZGFSPGQ
!     XABCR      O   ZZGFSPX
!     XBOD       O   ZZGFSPX
!     YREF       O   ZZGFSPX
!     XREF       O   ZZGFSPX
!     XOBS       O   ZZGFSPX
!     XRAD       O   ZZGFSPX
!     XSHP       O   ZZGFSPX
!
! $ Detailed_Input
!
!     OF       the string array naming the bodies whose angular
!              separation is of interest.
!
!     FROM     the string naming the observer.
!
!     SHAPE    the string array naming the geometric model used to
!              represent the shapes of OF. The relation between SHAPE
!              and OF is 1-to-1.
!
!              Models supported by this routine:
!
!                 'SPHERE'        Treat the body as a sphere with
!                                 radius equal to the maximum value of
!                                 BODYnnn_RADII
!
!                 'POINT'         Treat the body as a single point;
!                                 radius has value zero.
!
!              The SHAPE string lacks sensitivity to case and leading
!              or trailing blank.
!
!     FRAME    the string array naming the body-fixed reference frames
!              corresponding to OF. The relation between FRAME
!              and OF is 1-to-1.
!
!     ET       is the time in second past J2000 at which one wants
!              to determine an event condition.
!
!     ABCORR   the string description of the aberration corrections
!              to apply to the state evaluations to account for
!              one-way light time and stellar aberration.
!
!              This routine accepts the same aberration corrections
!              as does the SPICE routine SPKEZR. See the header of
!              SPKEZR for a detailed description of the aberration
!              correction options. For convenience, the options are
!              listed below:
!
!                 'NONE'     Apply no correction.
!
!                 'LT'       "Reception" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'LT+S'     "Reception" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'CN'       "Reception" case: converged
!                            Newtonian light time correction.
!
!                 'CN+S'     "Reception" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 'XLT'      "Transmission" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'XLT+S'    "Transmission" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'XCN'      "Transmission" case: converged
!                            Newtonian light time correction.
!
!                 'XCN+S'    "Transmission" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 The ABCORR string lacks sensitivity to case, leading
!                 and trailing blanks.
!
!     DECRES   is .TRUE. if the angular separation between the
!              objects is decreasing. Otherwise it is .FALSE.
!
!     SEP      is the angular separation between SVBOD1 and SVBOD2 as
!              seen from SVOBS at time ET.
!
!     For more information, see individual entry points.
!
! $ Detailed_Output
!
!     See individual entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If a direct call to ZZGFSPU occurs, the error
!         SPICE(BOGUSENTRY) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine serves as the umbrella routine for 4 entry points
!     needed by GFEVNT in solving for angular separation conditions.
!
!     The five entry points are
!
!        ZZGFSPIN  --- an initialization routine that must be called
!                     prior to attempting to solve for any angular
!                     separation event.
!
!        ZZGFSPDC --- determines whether or not angular separation is
!                     decreasing at some time.
!
!        ZZGFSPGQ --- returns the angular separation of the two
!                     objects of interest as a function of ET.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  ZZGFSPIN must be called prior to use of any of the
!         other entry points.
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
!-    SPICELIB Version 3.0.0, 31-MAY-2021 (EDW) (JDR)
!
!        Edited umbrella routine and all its entry points' headers to
!        comply with NAIF standard. Extended $Brief_I/O to list all
!        arguments and refer to corresponding entry points.
!
!        Moved UDFUNC declaration to $Declarations section.
!
!        Routine ZZGFSPGQ now calls ZZSEPQ, a copy of ZZGFSPQ cast as
!        a function.
!
!-    SPICELIB Version 2.0.0, 27-JUN-2012 (EDW)
!
!        Code edits to implement use of ZZGFRELX.
!        These edits include removal of unneeded routines:
!
!           ZZGFSPUR
!           ZZGFSPLT
!
!        and corresponding unused variables.
!
!        Routine ZZGFGSEP renamed to ZZGFSPGQ to match geometry finder
!        naming convention.
!
!        Implemented a proper $Exceptions section. Update to header
!        entries.
!
!-    SPICELIB Version 1.1.0, 29-DEC-2009 (NJB) (EDW)
!
!        Edited argument descriptions. Removed mention of "ELLIPSOID"
!        shape from SHAPE as that option is not yet implemented.
!
!        Added an error check on body frame centers to enforce
!        a body frame center is the body. This check does not apply
!        to "POINT" or "SPHERE" shape targets, and so will not
!        execute for this version of the routine.
!
!        Rename of the ZZDHA call to DHFA.
!
!-    SPICELIB Version 1.0.0, 19-FEB-2009 (NJB) (EDW)
!
!-&


! $ Index_Entries
!
!     umbrella routine for finding angular separation events
!
!-&


!
!     SPICELIB functions
!
      INTEGER               ISRCHC
      LOGICAL               FAILED
      LOGICAL               RETURN
      DOUBLE PRECISION      DVSEP
      DOUBLE PRECISION      DHFA
      DOUBLE PRECISION      ZZSEPQ

!
!     Local Variables
!
      DOUBLE PRECISION      AXES1 (3)
      DOUBLE PRECISION      AXES2 (3)

      DOUBLE PRECISION      PV1   (6)
      DOUBLE PRECISION      PV2   (6)

      DOUBLE PRECISION      DTHETA
      DOUBLE PRECISION      LT

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FOUND

      INTEGER               FCODE1
      INTEGER               FCODE2
      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               CTR1
      INTEGER               CTR2


!
!     Saved Variables
!
      CHARACTER*(32)        SVABCR
      SAVE                  SVABCR

      INTEGER               SVBOD1
      SAVE                  SVBOD1

      INTEGER               SVBOD2
      SAVE                  SVBOD2

      CHARACTER*(32)        SVREF
      SAVE                  SVREF

      CHARACTER*(32)        SVREF1
      SAVE                  SVREF1

      CHARACTER*(32)        SVREF2
      SAVE                  SVREF2

      INTEGER               SVOBS
      SAVE                  SVOBS

      DOUBLE PRECISION      SVRAD1
      SAVE                  SVRAD1

      DOUBLE PRECISION      SVRAD2
      SAVE                  SVRAD2

      INTEGER               SVSHP1
      SAVE                  SVSHP1

      INTEGER               SVSHP2
      SAVE                  SVSHP2

      CHARACTER*(5)         REF
      SAVE                  REF

!
!     Below we initialize the list of shape names.
!
      INTEGER               SPSHPN
      PARAMETER           ( SPSHPN = 2 )

      CHARACTER*(32)        SVSHAP ( SPSHPN )

!
!     Define integer ID parameters for the shape names in
!     SVSHAP.
!
      INTEGER               POINT
      PARAMETER           ( POINT = 1 )

      INTEGER               SPHERE
      PARAMETER           ( SPHERE = 2 )

      SAVE                  SVSHAP

      DATA                  SVSHAP / 'POINT', &
                                     'SPHERE' /

      DATA                  REF    / 'J2000'/

!
!     Never directly call this routine.
!
      CALL CHKIN ( 'ZZGFSPU'           )
      CALL SIGERR( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT( 'ZZGFSPU'           )

      RETURN




!$Procedure ZZGFSPIN ( Private - GF, angular separation initialization )

      ENTRY ZZGFSPIN ( OF, FROM, SHAPE, FRAME, ABCORR )

! $ Abstract
!
!     This routine initializes variables that describe an angular
!     separation event of interest for solution by ZZGFSOLV.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     ROOT
!
! $ Declarations
!
!      CHARACTER*(*)         OF   ( 2 )
!      CHARACTER*(*)         FROM
!      CHARACTER*(*)         SHAPE( 2 )
!      CHARACTER*(*)         FRAME( 2 )
!      CHARACTER*(*)         ABCORR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OF         I   Body id's of the angular separation objects
!     FROM       I   Observer name
!     SHAPE      I   Array of shape IDs corresponding to OF
!     FRAME      I   Array of frame names corresponding to OF
!     ABCORR     I   Aberration correction flag.
!
! $ Detailed_Input
!
!     OF       the string array naming the bodies whose angular
!              separation is of interest.
!
!     FROM     the string naming the observer.
!
!     SHAPE    the string array naming the geometric model used to
!              represent the shapes of OF. The relation between SHAPE
!              and OF is 1-to-1 and onto.
!
!              Models supported by this routine:
!
!                 'SPHERE'        Treat the body as a sphere with
!                                 radius equal to the maximum value of
!                                 BODYnnn_RADII
!
!                 'POINT'         Treat the body as a single point;
!                                 radius has value zero.
!
!              The SHAPE string lacks sensitivity to case and leading
!              or trailing blank.
!
!     FRAME    the string array naming the body-fixed reference frames
!              corresponding to OF. The relation between FRAME
!              and OF is 1-to-1.
!
!     ABCORR   the string description of the aberration corrections
!              to apply to the state evaluations to account for
!              one-way light time and stellar aberration.
!
!              This routine accepts the same aberration corrections
!              as does the SPICE routine SPKEZR. See the header of
!              SPKEZR for a detailed description of the aberration
!              correction options. For convenience, the options are
!              listed below:
!
!                 'NONE'     Apply no correction.
!
!                 'LT'       "Reception" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'LT+S'     "Reception" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'CN'       "Reception" case: converged
!                            Newtonian light time correction.
!
!                 'CN+S'     "Reception" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 'XLT'      "Transmission" case: correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'XLT+S'    "Transmission" case: correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'XCN'      "Transmission" case: converged
!                            Newtonian light time correction.
!
!                 'XCN+S'    "Transmission" case: converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 The ABCORR string lacks sensitivity to case, leading
!                 and trailing blanks.
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
!     1)  If the object name for target 1, OF(1), is not a recognized
!         name, the error SPICE(IDCODENOTFOUND) is signaled.
!
!     2)  If the object name for target 2, OF(2), is not a recognized
!         name, the error SPICE(IDCODENOTFOUND) is signaled.
!
!     3)  If the object name for the observer, FROM, is not a recognized
!         name, the error SPICE(IDCODENOTFOUND) is signaled.
!
!     4)  If the three objects associated with an ANGULAR SEPARATION
!         search are not distinct, the error SPICE(BODIESNOTDISTINCT) is
!         signaled.
!
!     5)  If the body shape for target 1, SHAPE(1), is not recognized,
!         the error SPICE(NOTRECOGNIZED) is signaled.
!
!     6)  If the SHAPE(1) value lacks a corresponding case block, the
!         error SPICE(BUG) is signaled. This indicates a programming
!         error.
!
!     7)  If the body shape for target 2, SHAPE(2), is not recognized,
!         the error SPICE(NOTRECOGNIZED) is signaled.
!
!     8)  If the SHAPE(2) value lacks a corresponding case block, the
!         error SPICE(BUG) is signaled. This indicates a programming
!         error.
!
!     9)  If frame subsystem did not recognize frame name FRAME(1), the
!         error SPICE(NOFRAME) is signaled.
!
!     10) If the reference frame associated with target body 1, OF(1),
!         is not centered on target body 1, the error
!         SPICE(INVALIDFRAME) is signaled.
!
!     11) If frame subsystem did not recognize frame name FRAME(2), the
!         error SPICE(NOFRAME) is signaled.
!
!     12) If the reference frame associated with target body 2, OF(2),
!         is not centered on target body 2, the error
!         SPICE(INVALIDFRAME) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 31-MAY-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 27-JUN-2012 (EDW)
!
!        REFVAL removed from routine argument list due to the use
!        of ZZGFRELX to calculate the events.
!
!        Implemented a proper $Exceptions section. Update to
!        $Author_and_Institution section.
!
!-    SPICELIB Version 1.1.0, 29-DEC-2009 (NJB) (EDW)
!
!        Edited argument descriptions. Removed mention of "ELLIPSOID"
!        shape from SHAPE as that option is not yet implemented.
!
!        Added an error check on body frame centers to enforce
!        a body frame center is the body. This check does not apply
!        to "POINT" or "SPHERE" shape targets, and so will not
!        execute for this version of the routine.
!
!-    SPICELIB Version 1.0.0, 14-APR-2008 (NJB) (EDW)
!
!-&


! $ Index_Entries
!
!     angular separation initialization routine
!
!-&


      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN( 'ZZGFSPIN' )
      END IF

      CALL BODS2C ( OF(1), SVBOD1, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG( 'The object name for target 1, ' &
            //         '''#'', is not a recognized name for an ' &
            //         'ephemeris object. The cause of this ' &
            //         'problem may be that you need an updated ' &
            //         'version of the SPICE Toolkit.'        )
         CALL ERRCH ( '#', OF( 1 )                            )
         CALL SIGERR( 'SPICE(IDCODENOTFOUND)'                 )
         CALL CHKOUT( 'ZZGFSPIN'                              )
         RETURN

      END IF


      CALL BODS2C ( OF(2), SVBOD2, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG( 'The object name for target 2, ' &
            //         '''#'', is not a recognized name for an ' &
            //         'ephemeris object. The cause of this ' &
            //         'problem may be that you need an updated ' &
            //         'version of the SPICE Toolkit.'        )
         CALL ERRCH ( '#', OF ( 2 )                           )
         CALL SIGERR( 'SPICE(IDCODENOTFOUND)'                 )
         CALL CHKOUT( 'ZZGFSPIN'                              )
         RETURN

      END IF


      CALL BODS2C ( FROM, SVOBS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The object name for the observer, ' &
            //            '''#'', is not a recognized name for an ' &
            //            'ephemeris object. The cause of this ' &
            //            'problem may be that you need an updated ' &
            //            'version of the SPICE Toolkit.'          )
         CALL ERRCH  ( '#', FROM                                   )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                     )
         CALL CHKOUT ( 'ZZGFSPIN'                                  )
         RETURN

      END IF


!
!     Confirm the three bodies have unique IDs.
!
      IF      (       ( SVOBS  .EQ. SVBOD1 ) &
                 .OR. ( SVOBS  .EQ. SVBOD2 ) &
                 .OR. ( SVBOD1 .EQ. SVBOD2) ) THEN

         CALL SETMSG( 'All three objects associated ' &
            //           'with an ANGULAR SEPARATION search must be ' &
            //           'distinct. The objects whose angular ' &
            //           'separation is of interest were # and #. ' &
            //           'The observer was #.'                      )
         CALL ERRINT( '#', SVBOD1                                   )
         CALL ERRINT( '#', SVBOD2                                   )
         CALL ERRINT( '#', SVOBS                                    )
         CALL SIGERR( 'SPICE(BODIESNOTDISTINCT)'                    )
         CALL CHKOUT( 'ZZGFSPIN'                                    )
         RETURN

      END IF

!
!     Squeeze all blanks out of the aberration correction
!     string; ensure the string is in upper case.
!
      CALL CMPRSS ( ' ', 0, ABCORR, SVABCR )
      CALL UCASE  ( SVABCR, SVABCR )

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      CALL ZZVALCOR ( SVABCR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFSPIN' )
         RETURN
      END IF

      SVREF  = REF
      SVREF1 = FRAME(1)
      SVREF2 = FRAME(2)

!
!     Check shapes...
!
      CALL LJUST ( SHAPE(1), SHAPE(1) )
      CALL UCASE ( SHAPE(1), SHAPE(1) )

!
!     If we pass the error check, then SHAPE(1) exists in SVSHAP.
!
      SVSHP1 = ISRCHC( SHAPE(1), SPSHPN, SVSHAP)

      IF ( SVSHP1 .EQ. 0 ) THEN

         CALL SETMSG ( 'The body shape, # is not recognized.  ' &
            //         'Supported quantities are: ' &
            //         'POINT, SPHERE.'                        )
         CALL ERRCH  ( '#', SHAPE(1)                           )
         CALL SIGERR ( 'SPICE(NOTRECOGNIZED)'                  )
         CALL CHKOUT ( 'ZZGFSPIN'                              )
         RETURN

      ELSE IF ( SVSHP1 .EQ. POINT ) THEN

         SVRAD1 = 0.D0

      ELSE IF ( SVSHP1 .EQ. SPHERE ) THEN

         CALL ZZGFTREB( SVBOD1, AXES1 )

         IF (  FAILED() ) THEN
            CALL CHKOUT( 'ZZGFSPIN' )
            RETURN
         END IF

         SVRAD1 = MAX( AXES1(1), AXES1(2), AXES1(3) )

      ELSE

!
!        This code executes only if someone adds a new shape
!        name to SVSHAP then fails to update the SVSHP1 condition
!        block to respond to the name. Fortran needs SWITCH...CASE.
!
         CALL SETMSG ('Encountered uncoded shape ID for #. '     // &
                      'This indicates a bug. Please contact NAIF.')
         CALL ERRCH  ('#', SHAPE(1)                               )
         CALL SIGERR ('SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZGFSPIN'                                 )
         RETURN

      END IF

      CALL LJUST ( SHAPE(2), SHAPE(2) )
      CALL UCASE ( SHAPE(2), SHAPE(2) )

!
!     If we pass the error check, then SHAPE(2) exists in SVSHAP.
!
      SVSHP2 = ISRCHC( SHAPE(2), SPSHPN, SVSHAP)

      IF ( SVSHP2 .EQ. 0 ) THEN

         CALL SETMSG ( 'The body shape, # is not '                 // &
                       'recognized.  Supported quantities are: '   // &
                       'POINT, SPHERE.'                             )
         CALL ERRCH ( '#', SHAPE(2)                                 )
         CALL SIGERR( 'SPICE(NOTRECOGNIZED)'                        )
         CALL CHKOUT( 'ZZGFSPIN'                                    )
         RETURN

      ELSE IF ( SVSHP2 .EQ. POINT ) THEN

         SVRAD2 = 0.D0

      ELSE IF ( SVSHP2 .EQ. SPHERE ) THEN

         CALL ZZGFTREB( SVBOD2, AXES2)

         IF (  FAILED() ) THEN
            CALL CHKOUT( 'ZZGFSPIN' )
            RETURN
         END IF

         SVRAD2 = MAX( AXES2(1), AXES2(2), AXES2(3) )

      ELSE

!
!        This code executes only if someone adds a new shape
!        name to SVSHAP then fails to update the SVSHP2 condition
!        block to respond to the name. Fortran needs SWITCH...CASE.
!
         CALL SETMSG ('Encountered uncoded shape ID for #. '     // &
                      'This indicates a bug. Please contact NAIF.')
         CALL ERRCH  ('#', SHAPE(2)                               )
         CALL SIGERR ('SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZGFSPIN'                                 )
         RETURN

      END IF


!
!     Confirm the center of the input reference frames correspond
!     to the target bodies for non-point, non-spherical bodies.
!
!        FRAME1 centered on TARG1
!        FRAME2 centered on TARG2
!
!     This check does not apply to POINT or SPHERE shapes.
!

      IF ( (SVSHP1 .NE. POINT)  .AND. (SVSHP1 .NE. SPHERE) ) THEN

         CALL NAMFRM ( SVREF1, FCODE1 )

         CALL FRINFO ( FCODE1, CTR1, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', SVREF1                               )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZGFSPIN'                                )
            RETURN

         END IF

         IF ( SVBOD1 .NE. CTR1 ) THEN

            CALL SETMSG ( 'The reference frame #1 associated with ' // &
                      'target body #2 is not centered on #2. The '  // &
                      'frame must be centered on the target body.' )

            CALL ERRCH  ( '#1', SVREF1                     )
            CALL ERRCH  ( '#2', OF(1)                      )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
            CALL CHKOUT ( 'ZZGFSPIN'                       )
            RETURN

         END IF

      END IF


      IF ( (SVSHP2 .NE. POINT)  .AND. (SVSHP2 .NE. SPHERE) ) THEN

         CALL NAMFRM ( SVREF2, FCODE2 )

         CALL FRINFO ( FCODE2, CTR2, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', SVREF2                               )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZGFSPIN'                                )
            RETURN

         END IF

         IF ( SVBOD2 .NE. CTR2 ) THEN

            CALL SETMSG ( 'The reference frame #1 associated with ' // &
                      'target body #2 is not centered on #2. The '  // &
                      'frame must be centered on the target body.' )

            CALL ERRCH  ( '#1', SVREF2                     )
            CALL ERRCH  ( '#2', OF(2)                      )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
            CALL CHKOUT ( 'ZZGFSPIN'                       )
            RETURN

         END IF

      END IF

      CALL CHKOUT( 'ZZGFSPIN' )
      RETURN




!$Procedure ZZGFSPDC ( Private - GF, angular separation decreasing)

      ENTRY ZZGFSPDC ( UDFUNC, ET, DECRES )

! $ Abstract
!
!     Computes whether or not the angular separation between SVBOD1 and
!     SVBOD2 is decreasing at time ET.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     ROOT
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!     LOGICAL               DECRES
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     DECRES     O   .TRUE. if angular separation is decreasing .FALSE.
!                    otherwise.
!
! $ Detailed_Input
!
!     ET       time in seconds past J2000 at which one wishes to
!              determine whether or not the angular separation of the
!              two bodies is decreasing.
!
! $ Detailed_Output
!
!     DECRES   is .TRUE. if the angular separation between the objects
!              is decreasing. Otherwise it is .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the observer is inside one of the objects, the object will
!         be regarded as having a 90 degree apparent radius.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     A function f(x) is strictly decreasing at x0 if and only if there
!     exists some delta > 0 such that for all dx satisfying
!
!        0  <  dx  < delta
!
!     we have
!
!        f(x0)       <  f(x0 + dx)
!
!     and
!
!        f(x0 - dx)  <  f(x)
!
!     Note that a strictly decreasing function need not be
!     differentiable in a neighborhood of x0; it can have jump
!     discontinuities in any neighborhood of x0 and even at x0.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 30-MAY-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 18-FEB-2011 (EDW)
!
!        Added UDFUNC to argument list for use of ZZGFRELX when
!        calculating the events.
!
!-    SPICELIB Version 1.0.1, 06-JUL-2009 (NJB) (EDW)
!
!        Rename of the ZZDHA call to DHFA.
!
!-    SPICELIB Version 1.0.0, 29-APR-2008 (NJB)
!
!-&


! $ Index_Entries
!
!     angular separation is decreasing
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN( 'ZZGFSPDC' )
      END IF

      CALL SPKEZ ( SVBOD1, ET, SVREF, SVABCR, SVOBS, PV1, LT )

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFSPDC' )
         RETURN
      END IF

      CALL SPKEZ ( SVBOD2, ET, SVREF, SVABCR, SVOBS, PV2, LT )

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFSPDC' )
         RETURN
      END IF

!
!     The angular separation between the bodies has the value
!
!        theta = sep - alpha1 - alpha2
!
!     With alpha1 the half angle of SVBOD1, alpha2 the half
!     angle of SVBOD2, half angle defined as (for spheres):
!
!        sin(alpha) = body_radius
!                     -----------
!                     range_to_body
!
!     The corresponding time derivative of theta:
!
!        d(theta) = d(sep) - d(alpha1) - d(alpha2)
!        --------   ------   ---------   ---------
!        dt         dt       dt          dt
!
!     Note, alpha1, alpha2 and their derivatives have value zero
!     for point objects.
!
      DTHETA = DVSEP(PV1, PV2)

!
!     Check for a failure caused by a numerical event.
!
      IF (  FAILED() ) THEN
         DECRES = .TRUE.
         CALL CHKOUT ( 'ZZGFSPDC' )
         RETURN
      END IF

      DTHETA = DTHETA - DHFA(PV1, SVRAD1) - DHFA(PV2, SVRAD2)

      IF ( DTHETA .LT. 0 ) THEN
         DECRES = .TRUE.
      ELSE
         DECRES = .FALSE.
      END IF

      CALL CHKOUT ( 'ZZGFSPDC' )
      RETURN




!$Procedure ZZGFSPGQ ( Private - GF, calculate angular separation )

      ENTRY ZZGFSPGQ ( ET, SEP )

! $ Abstract
!
!     Determine the angular separation between the limbs of the two
!     bodies.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     ROOT
!
! $ Declarations
!
!      DOUBLE PRECISION      ET
!      DOUBLE PRECISION      SEP
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     SEP        O   Separation at time ET.
!
! $ Detailed_Input
!
!     ET       time in ephemeris seconds past J2000 when the
!              angular separation between the two bodies is
!              to be computed.
!
! $ Detailed_Output
!
!     SEP      is the angular separation between SVBOD1 and SVBOD2 as
!              seen from SVOBS at time ET.
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
!     This routine determines the apparent angular separation between
!     the limbs of bodies SVBOD1 and SVBOD2 as seen from SVOBS at
!     time ET.
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
!     J. Diaz del Rio    (ODC Space)
!     L.S. Elson         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.0, 30-MAY-2021 (EDW) (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Routine now calls ZZSEPQ, a copy of ZZGFSPQ cast as
!        a function.
!
!-    SPICELIB Version 2.0.0, 17-FEB-2011 (EDW)
!
!        Routine renamed from ZZGFGSEP to ZZGFSPGQ to match geometry
!        finder naming convention.
!
!-    SPICELIB Version 1.0.0, 26-AUG-2003 (LSE)
!
!-&


! $ Index_Entries
!
!     angular separation between two bodies
!
!-&


      SEP = ZZSEPQ ( ET, &
                     SVBOD1, SVBOD2, &
                     SVRAD1, SVRAD2, &
                     SVOBS,  SVABCR, SVREF )

      RETURN




!$Procedure ZZGFSPX ( Private -- GF, retrieve ZZGFSPIN values )

      ENTRY ZZGFSPX ( XABCR, XBOD, YREF, XREF, &
                      XOBS,  XRAD, XSHP )

! $ Abstract
!
!     Retrieve values set in ZZGFSPIN.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations
!
!     CHARACTER*(*)         XABCR
!     INTEGER               XBOD  (2)
!     CHARACTER*(*)         YREF
!     CHARACTER*(*)         XREF  (2)
!     INTEGER               XOBS
!     DOUBLE PRECISION      XRAD  (2)
!     INTEGER               XSHP  (2)
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     XABCR      O   Saved value SVABCR.
!     XBOD       O   Saved values SVBOD1 and SVBOD2.
!     YREF       O   Saved value SVREF.
!     XREF       O   Saved values SVREF1 and SVREF2.
!     XOBS       O   Saved value SVOBS.
!     XRAD       O   Saved values SVRAD1 and SVRAD2.
!     XSHP       O   Saved values SVSHP1 and SVSHP2.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     XABCR    initialized via ZZGFSPIN.
!
!     XBOD1    initialized via ZZGFSPIN.
!
!     XBOD2    initialized via ZZGFSPIN.
!
!     YREF     initialized via ZZGFSPIN.
!
!     XREF1    initialized via ZZGFSPIN.
!
!     XREF2    initialized via ZZGFSPIN.
!
!     XOBS     initialized via ZZGFSPIN.
!
!     XRAD1    initialized via ZZGFSPIN.
!
!     XRAD2    initialized via ZZGFSPIN.
!
!     XSHP1    initialized via ZZGFSPIN.
!
!     XSHP2    initialized via ZZGFSPIN.
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
!     None.
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
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 30-MAY-2021 (EDW) (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Completed descriptions for $Brief_I/O.
!
!-    SPICELIB Version 1.0.0, 24-SEP-2012 (EDW)
!
!-&


! $ Index_Entries
!
!     get saved separation angle parameters
!
!-&


      XABCR   = SVABCR
      XBOD(1) = SVBOD1
      XBOD(2) = SVBOD2
      YREF    = SVREF
      XREF(1) = SVREF1
      XREF(2) = SVREF2
      XOBS    = SVOBS
      XRAD(1) = SVRAD1
      XRAD(2) = SVRAD2
      XSHP(1) = SVSHP1
      XSHP(2) = SVSHP2

      RETURN
      END
