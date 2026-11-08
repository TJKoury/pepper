!$Procedure ZZSPIN ( Separation quantity initializer )

      SUBROUTINE ZZSPIN ( TARG1,  SHAPE1, FRAME1, &
                          TARG2,  SHAPE2, FRAME2, &
                          OBSRVR, ABCORR, &
                          BODS,   FRAMES, MXRAD, OBS )

! $ Abstract
!
!     Check/initialize the quantities describing an angular separation
!     between two spherical or point objects.
!
! $ Required_Reading
!
!     ABCORR.REQ
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         TARG1
      CHARACTER*(*)         SHAPE1
      CHARACTER*(*)         FRAME1
      CHARACTER*(*)         TARG2
      CHARACTER*(*)         SHAPE2
      CHARACTER*(*)         FRAME2
      CHARACTER*(*)         OBSRVR
      CHARACTER*(*)         ABCORR
      INTEGER               BODS   (2)
      INTEGER               FRAMES (2)
      DOUBLE PRECISION      MXRAD  (2)
      INTEGER               OBS

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TARG1      I   Name for first target
!     SHAPE1     I   Shape corresponding to TARG1
!     FRAME1     I   Body fixed frame for TARG1
!     TARG2      I   Name for second target
!     SHAPE2     I   Shape corresponding to TARG2
!     FRAME2     I   Body fixed frame for TARG2
!     OBSRVR     I   Name for observer body
!     ABCORR     I   Aberration correction flag
!     BODS       O   Array(2) of body/SPK IDs for TARG1 and TARG2
!     FRAMES     O   Array(2) of frame IDs for TARG1 and TARG2
!     MXRAD      O   Array(2) of max radii values for TARG1 and TARG2
!     OBS        O   Body/SPK IDs of OBSRVR
!
! $ Detailed_Input
!
!     TARG1    the string naming the first body of interest. You can
!              also supply the integer ID code for the object as an
!              integer string. For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!     SHAPE1   the string naming the geometric model used to
!              represent the shape of the TARG1 body. Models
!              supported by this routine:
!
!                'SPHERE'        Treat the body as a sphere with
!                                radius equal to the maximum value of
!                                BODYnnn_AXES.
!
!                'POINT'         Treat the body as a point;
!                                radius has value zero.
!
!              The SHAPE1 string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     FRAME1   the string naming the body-fixed reference frame
!              corresponding to TARG1. This routine does not currently
!              use this argument's value, its use is reserved for
!              future shape models. The value 'NULL' will suffice for
!              "POINT" and "SPHERE" shaped bodies.
!
!     TARG2    the string naming the second body of interest. You can
!              also supply the integer ID code for the object as an
!              integer string. For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!     SHAPE2   the string naming the geometric model used to
!              represent the shape of the TARG2 body. Models
!              supported by this routine:
!
!                'SPHERE'        Treat the body as a sphere with
!                                radius equal to the maximum value of
!                                BODYnnn_AXES.
!
!                'POINT'         Treat the body as a point;
!                                radius has value zero.
!
!              The SHAPE2 string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     FRAME2   the string naming the body-fixed reference frame
!              corresponding to TARG2. This routine does not currently
!              use this argument's value, its use is reserved for
!              future shape models. The value 'NULL' will suffice for
!              "POINT" and "SPHERE" shaped bodies.
!
!     OBSRVR   the string naming the observing body. Optionally, you
!              may supply the ID code of the object as an integer
!              string. For example, both 'EARTH' and '399' are
!              legitimate strings to supply to indicate the
!              observer is Earth.
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
!     BODS     the NAIF SPK/body IDs of the two objects for which to
!              determine the angular separation.
!
!     FRAMES   the NAIF frame IDs of the body fixed reference frames
!              of two objects for which to determine the angular
!              separation.
!
!     MXRAD    the maximum radial values of the two objects.
!
!     OBS      the NAIF SPK/body ID of the observer.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the three objects TARG1, TARG2, and OBSRVR are not
!         distinct, the error SPICE(BODIESNOTDISTINCT) is signaled.
!
!     2)  If the SHAPE1 or SHAPE2 values lack a corresponding case
!         block, the error SPICE(BUG) is signaled. This indicates a
!         programming error.
!
!     3)  If the object names for TARG1 TARG2, or OBSRVR cannot resolve
!         to a NAIF body ID, the error SPICE(IDCODENOTFOUND) is
!         signaled.
!
!     4)  If the reference frame associated with TARG1, FRAME1, is not
!         centered on TARG1, or if the reference frame associated with
!         TARG2, FRAME2, is not centered on TARG2, the error
!         SPICE(INVALIDFRAME) is signaled.
!
!     5)  If the frame name for FRAME1 or FRAME2 cannot resolve to a
!         NAIF frame ID, the error SPICE(NOFRAME) is signaled.
!
!     6)  If the body shape for TARG1, SHAPE1, or the body shape for
!         TARG2, SHAPE2, is not recognized, the error
!         SPICE(NOTRECOGNIZED) is signaled.
!
!     7)  If the requested aberration correction ABCORR is not
!         recognized, an error is signaled by a routine in the call tree
!         of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Please refer to the Aberration Corrections Required Reading
!     (abcorr.req) for detailed information describing the nature and
!     calculation of the applied corrections.
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
!-    SPICELIB Version 1.0.0, 16-JAN-2021 (EDW) (JDR)
!
!        Based on code originally in zzgfspu.f.
!
!-&


! $ Index_Entries
!
!     check quantities for apparent relative angular separation
!
!-&


!
!     SPICELIB functions.
!
      INTEGER               ISRCHC

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               FOUND

!
!     Local parameters
!
      INTEGER               SHAPLN
      PARAMETER           ( SHAPLN = 6 )


!
!     Local Variables
!
      CHARACTER*(SHAPLN)    SHAP1
      CHARACTER*(SHAPLN)    SHAP2

      DOUBLE PRECISION      AXES1 (3)
      DOUBLE PRECISION      AXES2 (3)

      INTEGER               CLASS
      INTEGER               CLSSID
      INTEGER               CTR1
      INTEGER               CTR2
      INTEGER               SHP1
      INTEGER               SHP2

      LOGICAL               ATTBLK ( NABCOR )

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

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN( 'ZZSPIN' )

!
!     FRAMES argument not currently used.
!
      FRAMES(1) = -1
      FRAMES(2) = -1

      CALL BODS2C ( TARG1, BODS(1), FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG( 'The object name for target 1, ' &
            //         '''#'', is not a recognized name for an ' &
            //         'ephemeris object. The cause of this ' &
            //         'problem may be that you need an updated ' &
            //         'version of the SPICE Toolkit.'        )
         CALL ERRCH ( '#', TARG1                              )
         CALL SIGERR( 'SPICE(IDCODENOTFOUND)'                 )
         CALL CHKOUT( 'ZZSPIN'                                   )
         RETURN

      END IF


      CALL BODS2C ( TARG2, BODS(2), FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG( 'The object name for target 2, ' &
            //         '''#'', is not a recognized name for an ' &
            //         'ephemeris object. The cause of this ' &
            //         'problem may be that you need an updated ' &
            //         'version of the SPICE Toolkit.'        )
         CALL ERRCH ( '#', TARG2                              )
         CALL SIGERR( 'SPICE(IDCODENOTFOUND)'                 )
         CALL CHKOUT( 'ZZSPIN'                                )
         RETURN

      END IF


      CALL BODS2C ( OBSRVR, OBS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The object name for the observer, ' &
            //            '''#'', is not a recognized name for an ' &
            //            'ephemeris object. The cause of this ' &
            //            'problem may be that you need an updated ' &
            //            'version of the SPICE Toolkit.'          )
         CALL ERRCH  ( '#', OBSRVR                                 )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                     )
         CALL CHKOUT ( 'ZZSPIN'                                    )
         RETURN

      END IF


!
!     Confirm the three bodies have unique IDs.
!
      IF      (       ( OBS     .EQ. BODS(1) ) &
                 .OR. ( OBS     .EQ. BODS(2) ) &
                 .OR. ( BODS(1) .EQ. BODS(2)) ) THEN

         CALL SETMSG( 'All three objects associated ' &
            //        'with an ANGULAR SEPARATION calculation ' &
            //        'must be distinct. The objects whose angular ' &
            //        'separation is of interest were # and #. ' &
            //        'The observer was #.'                       )
         CALL ERRINT( '#', BODS(1)                                )
         CALL ERRINT( '#', BODS(2)                                )
         CALL ERRINT( '#', OBS                                    )
         CALL SIGERR( 'SPICE(BODIESNOTDISTINCT)'                  )
         CALL CHKOUT( 'ZZSPIN'                                    )
         RETURN

      END IF

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      CALL ZZVALCOR ( ABCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZSPIN' )
         RETURN
      END IF


!
!     Check first shape.
!
      CALL LJUCRS ( 1, SHAPE1, SHAP1 )

!
!     If we pass the error check, then SHAPE1 exists in SVSHAP.
!
      SHP1 = ISRCHC( SHAP1, SPSHPN, SVSHAP)

      IF ( SHP1 .EQ. 0 ) THEN

         CALL SETMSG ( 'The body shape, # is not recognized.  ' &
            //         'Supported quantities are: ' &
            //         'POINT, SPHERE.'                      )
         CALL ERRCH  ( '#', SHAP1                            )
         CALL SIGERR ( 'SPICE(NOTRECOGNIZED)'                )
         CALL CHKOUT ( 'ZZSPIN'                              )
         RETURN

      ELSE IF ( SHP1 .EQ. POINT ) THEN

         MXRAD(1) = 0.D0

      ELSE IF ( SHP1 .EQ. SPHERE ) THEN

         CALL ZZGFTREB( BODS(1), AXES1 )

         IF (  FAILED() ) THEN
            CALL CHKOUT( 'ZZSPIN' )
            RETURN
         END IF

         MXRAD(1) = MAX( AXES1(1), AXES1(2), AXES1(3) )

      ELSE

!
!        This code executes only if someone adds a new shape
!        name to SVSHAP then fails to update the SHP1 condition
!        block to respond to the name. Fortran needs SWITCH...CASE.
!
         CALL SETMSG ('Encountered uncoded shape ID for #. '     // &
                      'This indicates a bug. Please contact NAIF.')
         CALL ERRCH  ('#', SHAP1                                  )
         CALL SIGERR ('SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZSPIN'                                   )
         RETURN

      END IF

!
!     Check second shape.
!
      CALL LJUCRS ( 1, SHAPE2, SHAP2 )

!
!     If we pass the error check, then SHAPE2 exists in SVSHAP.
!
      SHP2 = ISRCHC( SHAP2, SPSHPN, SVSHAP)

      IF ( SHP2 .EQ. 0 ) THEN

         CALL SETMSG ( 'The body shape, # is not '                 // &
                       'recognized.  Supported quantities are: '   // &
                       'POINT, SPHERE.'                             )
         CALL ERRCH ( '#', SHAP2                                    )
         CALL SIGERR( 'SPICE(NOTRECOGNIZED)'                        )
         CALL CHKOUT( 'ZZSPIN'                                      )
         RETURN

      ELSE IF ( SHP2 .EQ. POINT ) THEN

         MXRAD(2) = 0.D0

      ELSE IF ( SHP2 .EQ. SPHERE ) THEN

         CALL ZZGFTREB( BODS(2), AXES2 )

         IF (  FAILED() ) THEN
            CALL CHKOUT( 'ZZSPIN' )
            RETURN
         END IF

         MXRAD(2) = MAX( AXES2(1), AXES2(2), AXES2(3) )

      ELSE

!
!        This code executes only if someone adds a new shape
!        name to SVSHAP then fails to update the SHP2 condition
!        block to respond to the name. Fortran needs SWITCH...CASE.
!
         CALL SETMSG ('Encountered uncoded shape ID for #. '     // &
                      'This indicates a bug. Please contact NAIF.')
         CALL ERRCH  ('#', SHAP2                                  )
         CALL SIGERR ('SPICE(BUG)'                                )
         CALL CHKOUT ( 'ZZSPIN'                                   )
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

      IF ( (SHP1 .NE. POINT)  .AND. (SHP1 .NE. SPHERE) ) THEN

         CALL NAMFRM ( FRAME1, FRAMES(1) )

         CALL FRINFO ( FRAMES(1), CTR1, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', FRAME1                               )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZSPIN'                                  )
            RETURN

         END IF

         IF ( BODS(1) .NE. CTR1 ) THEN

            CALL SETMSG ( 'The reference frame #1 associated with ' // &
                      'target body #2 is not centered on #2. The '  // &
                      'frame must be centered on the target body.' )

            CALL ERRCH  ( '#1', FRAME1                     )
            CALL ERRCH  ( '#2', TARG1                      )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
            CALL CHKOUT ( 'ZZSPIN'                         )
            RETURN

         END IF

      END IF


      IF ( (SHP2 .NE. POINT)  .AND. (SHP2 .NE. SPHERE) ) THEN

         CALL NAMFRM ( FRAME2, FRAMES(2) )

         CALL FRINFO ( FRAMES(2), CTR2, CLASS, CLSSID, FOUND )

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Frame system did not recognize frame #.' )
            CALL ERRCH  ( '#', FRAME2                               )
            CALL SIGERR ( 'SPICE(NOFRAME)'                          )
            CALL CHKOUT ( 'ZZSPIN'                                  )
            RETURN

         END IF

         IF ( BODS(2) .NE. CTR2 ) THEN

            CALL SETMSG ( 'The reference frame #1 associated with ' // &
                      'target body #2 is not centered on #2. The '  // &
                      'frame must be centered on the target body.' )

            CALL ERRCH  ( '#1', FRAME2                     )
            CALL ERRCH  ( '#2', TARG2                      )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'            )
            CALL CHKOUT ( 'ZZSPIN'                         )
            RETURN

         END IF

      END IF

!
!     Done. Check out. Return.
!

      CALL CHKOUT ('ZZSPIN')
      RETURN
      END
