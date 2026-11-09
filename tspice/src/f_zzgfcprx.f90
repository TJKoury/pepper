!$Procedure      F_ZZGFCPRX ( Test coordinate derivative proxy routine )

      SUBROUTINE F_ZZGFCPRX ( OK )
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Test the GF private coordinate derivative proxy routine ZZGFCPRX.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
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
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the private SPICELIB GF
!     coordinate derivative proxy routine ZZGFCPRX.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 27-JUN-2008 (NJB)
!
!        Added new test cases:
!
!           - Handling of invalid coordinate system
!           - Handling of the rectangular system
!           - Handling of the DPSIGN==0 case where
!             the position is on the Z-axis
!           - Handling of the Z-velocity == 0 case
!             for cylindrical coordinates, where the
!             position is on the Z-axis
!           - Zero input velocity
!
!-    SPICELIB Version 1.0.0, 11-MAY-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDOT

!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgfcprx.tpc' )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

!
!     Small delta value used for near 0 and pi
!     latitudes.
!
      DOUBLE PRECISION      EPSLAT
      PARAMETER           ( EPSLAT = 1.D-1 )

      DOUBLE PRECISION      TWEAK
      PARAMETER           ( TWEAK  = 1.D-3 )

!
!     Number of recognized coordinate systems.
!
      INTEGER               NSYS
      PARAMETER           ( NSYS   = 7 )

!
!     Maximum length of a coordinate system name.
!
      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 32 )

!
!     Maximum length of a coordinate name.
!
      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )

!
!     Numbers of latitude and longitude samples.
!
      INTEGER               NLATSM
      PARAMETER           ( NLATSM = 5 )

      INTEGER               NLONSM
      PARAMETER           ( NLONSM = 8 )

!
!     Local Variables
!
      CHARACTER*(CRDLEN)    CRDNMS ( 3, NSYS )
      CHARACTER*(CRDLEN)    CRDSYS
      CHARACTER*(SYSLEN)    SYSNMS (    NSYS )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(LNSIZE)    QNAME

      DOUBLE PRECISION      DELTLA
      DOUBLE PRECISION      DELTLO
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      DP
      DOUBLE PRECISION      F
      DOUBLE PRECISION      JACOBI ( 3, 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      XVEL   ( 3 )

      INTEGER               CDSIGN ( 3 )
      INTEGER               CI
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NDLAT
      INTEGER               NDLON
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               SENSE
      INTEGER               SI

!
!     Save everything.
!
      SAVE

!
!     Initial values
!

!
!     Names of supported coordinate systems.
!
!     The Ith coordinate system in the array SYSNMS has coordinates
!     in the Ith row of the array CRDNMS. This association must be
!     preserved when this routine is updated.
!
      DATA                  SYSNMS /  RECSYS, &
                                      LATSYS, &
                                      RADSYS, &
                                      SPHSYS, &
                                      CYLSYS, &
                                      GEOSYS, &
                                      PGRSYS  /

!
!     Names of coordinate triples for the supported coordinate
!     systems.
!
!     The order of the coordinate names in the Ith row of this array
!     matches the order of the outputs of the corresponding
!     SPICELIB routine REC*, which maps a Cartesian vector to
!     the Ith coordinate system in the array SYSNMS. Again, this
!     order must be preserved.
!
      DATA                  CRDNMS /  XCRD,     YCRD,     ZCRD, &
                                      RADCRD,   LONCRD,   LATCRD, &
                                      RNGCRD,   RACRD,    DECCRD, &
                                      RADCRD,   CLTCRD,   LONCRD, &
                                      RADCRD,   LONCRD,   ZCRD, &
                                      LONCRD,   LATCRD,   ALTCRD, &
                                      LONCRD,   LATCRD,   ALTCRD  /



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFCPRX' )




!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Setup: create and load PCK file.' )
!
!     Load a PCK.
!
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad coordinate system' )

!
!     Set shape and spin parameters.
!
      RE    = 1.0D5
      F     = 0.5D0
      SENSE = -1

      CALL FILLD ( 1.D0, 6, STATE )

      CALL ZZGFCPRX ( STATE, 'GAUSSIAN', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad longitude sense' )

!
!     Set shape and spin parameters.
!
      RE    = 1.0D5
      F     = 0.5D0
      SENSE = -2

      CALL FILLD ( 1.D0, 6, STATE )

      CALL ZZGFCPRX ( STATE, 'PLANETOGRAPHIC', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad equatorial radius' )

!
!     Set shape and spin parameters.
!
      RE    = -1.0D5
      F     = 0.5D0
      SENSE = 1

      CALL FILLD ( 1.D0, 6, STATE )

      CALL ZZGFCPRX ( STATE, 'PLANETOGRAPHIC', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL ZZGFCPRX ( STATE, 'GEODETIC', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad flattening coefficient' )

!
!     Set shape and spin parameters.
!
      RE    = 1.0D5
      F     = 1.0D6
      SENSE = 1

      CALL FILLD ( 1.D0, 6, STATE )

      CALL ZZGFCPRX ( STATE, 'PLANETOGRAPHIC', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL ZZGFCPRX ( STATE, 'GEODETIC', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!*********************************************************************
!*
!*    Non-error Exceptional cases
!*
!*********************************************************************



!
!     Test handling of zero velocity vector.
!
      CALL TCASE ( 'Input velocity is zero; position is ' &
      //           'off Z-axis.'                          )

      CALL VPACK  ( 1.D0, 1.D0, 1.D0, STATE )
      CALL CLEARD ( 3, STATE(4) )

      DO SI = 1, NSYS
!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Zero velocity; system is #'

         CALL REPMC ( TITLE, '#', SYSNMS(SI), TITLE )

         CALL TCASE ( TITLE )

         CALL ZZGFCPRX ( STATE, SYSNMS(SI), 1.D0, 0.D0, 1, CDSIGN )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         DO CI = 1, 3

            QNAME = 'sign of d('
            CALL SUFFIX ( CRDNMS(CI,SI), 0, QNAME )
            CALL SUFFIX ( ')/dt',        0, QNAME )

            CALL CHCKSI ( QNAME, CDSIGN(CI), '=',  0, 0, OK )

         END DO

      END DO



!     Test handling of zero velocity vector.
!
      CALL TCASE ( 'Input velocity is zero; position is ' &
      //           'on Z-axis.'                          )

      CALL VPACK  ( 0.D0, 0.D0, 1.D0, STATE )
      CALL CLEARD ( 3, STATE(4) )

      DO SI = 1, NSYS
!
!---- Case -------------------------------------------------------------
!
         TITLE = 'Zero velocity; system is #'

         CALL REPMC ( TITLE, '#', SYSNMS(SI), TITLE )

         CALL TCASE ( TITLE )

         CALL ZZGFCPRX ( STATE, SYSNMS(SI), 1.D0, 0.D0, 1, CDSIGN )
         CALL CHCKXC   ( .FALSE., ' ', OK )

         DO CI = 1, 3

            QNAME = 'sign of d('
            CALL SUFFIX ( CRDNMS(CI,SI), 0, QNAME )
            CALL SUFFIX ( ')/dt',        0, QNAME )

            CALL CHCKSI ( QNAME, CDSIGN(CI), '=',  0, 0, OK )

         END DO

      END DO



!
!     Test results for positions on the Z-axis, with velocity
!     orthogonal to Z. Applies only to the cylindrical system.
!
!
!---- Case -------------------------------------------------------------
!
      TITLE = 'velocity orthogonal to Z; system is #'

      CALL REPMC ( TITLE, '#', CYLSYS, TITLE )

      CALL TCASE ( TITLE )

      CALL VPACK  ( 0.D0, 0.D0, 1.D0, STATE    )
      CALL VPACK  ( 1.D0, 0.D0, 0.D0, STATE(4) )


      CALL ZZGFCPRX ( STATE, CYLSYS, 1.D0, 0.D0, 1, CDSIGN )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO CI = 1, 3
!
!        Note: the index of the cylindrical system is 5.
!
         QNAME = 'sign of d('
         CALL SUFFIX ( CRDNMS(CI,5), 0, QNAME )
         CALL SUFFIX ( ')/dt',       0, QNAME )

         CALL CHCKSI ( QNAME, CDSIGN(CI), '=',  0, 0, OK )

      END DO


!
!     Test results for range or altitude coordinates for positions on
!     the Z-axis, with velocity orthogonal to Z. Applies to all but
!     rectangular systems.
!
!     The rectangular system has index 1, so start at index 2.
!
      DO SI = 2, NSYS
!
!---- Case -------------------------------------------------------------
!
         CRDSYS = SYSNMS(SI)

         TITLE = 'velocity orthogonal to Z; system is #'

         CALL REPMC ( TITLE, '#', CRDSYS, TITLE )

         CALL TCASE ( TITLE )

         CALL VPACK  ( 0.D0, 0.D0, 1.D0, STATE    )
         CALL VPACK  ( 1.D0, 0.D0, 0.D0, STATE(4) )


         CALL ZZGFCPRX ( STATE, CYLSYS, 1.D0, 0.D0, 1, CDSIGN )
         CALL CHCKXC   ( .FALSE., ' ', OK )

!
!        Set CI to the index of the radius or altitude coordinate
!        of the current system.
!
         IF ( CRDSYS .EQ. LATSYS ) THEN
            CI = 1
         ELSE IF ( CRDSYS .EQ. GEOSYS ) THEN
            CI = 3
         ELSE IF ( CRDSYS .EQ. RADSYS ) THEN
            CI = 1
         ELSE IF ( CRDSYS .EQ. PGRSYS ) THEN
            CI = 3
         ELSE IF ( CRDSYS .EQ. CYLSYS ) THEN
            CI = 1
         ELSE IF ( CRDSYS .EQ. SPHSYS ) THEN
            CI = 1
         ELSE
            CALL SETMSG ( 'Unexpected system: #' )
            CALL ERRCH  ( '#', CRDSYS            )
            CALL SIGERR ( 'SPICE(BUG)'           )
            CALL CHCKXC ( .FALSE., ' ', OK       )
         END IF

         QNAME = 'sign of d('
         CALL SUFFIX ( CRDNMS(CI,SI), 0, QNAME )
         CALL SUFFIX ( ')/dt',        0, QNAME )

         CALL CHCKSI ( QNAME, CDSIGN(CI), '=',  0, 0, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!

!
!     Test results for positions on the Z-axis.
!
      DO NLAT = 1, 3
!
!        Generate Z values corresponding to
!
!           +1, 0, -1
!
!        positions, respectively.
!
         STATE(1) = 0.D0
         STATE(2) = 0.D0
         STATE(3) = 2.D0 - DBLE(NLAT)


         DO NDLAT = 1, 2
!
!           Generate latitude values corresponding to
!
!              +Pi/4, -Pi/4
!
!           positions, respectively.
!
            DLAT = (3 - 2*NDLAT) * PI()/4.D0
            DLON = PI()/4.D0

            CALL LATREC ( 1.D0, DLON, DLAT, STATE(4) )

            DP = VDOT ( STATE, STATE(4) )
!
!           Check the coordinate derivative signs for
!           each coordinate system.
!
            DO SI = 1, NSYS
!
!---- Case -------------------------------------------------------------
!
               TITLE = '#; Z #; dlat #(deg); dlon #(deg)'

               CALL REPMC ( TITLE, '#', SYSNMS(SI),     TITLE )
               CALL REPMD ( TITLE, '#', STATE(3),    3, TITLE )
               CALL REPMD ( TITLE, '#', DPR()*DLAT,  3, TITLE )
               CALL REPMD ( TITLE, '#', DPR()*DLON,  3, TITLE )

               CALL TCASE ( TITLE )


               IF ( SYSNMS(SI) .EQ. RECSYS ) THEN

                  DO I = 1, 3
                     XVEL(I) = STATE(3+I)
                  END DO

               ELSE IF ( SYSNMS(SI) .EQ. LATSYS ) THEN

                  XVEL(1) = DP
                  XVEL(2) = 0.D0
                  XVEL(3) = 0.D0

               ELSE IF ( SYSNMS(SI) .EQ. RADSYS ) THEN

                  XVEL(1) = DP
                  XVEL(2) = 0.D0
                  XVEL(3) = 0.D0

               ELSE IF ( SYSNMS(SI) .EQ. SPHSYS ) THEN

                  XVEL(1) = DP
                  XVEL(2) = 0.D0
                  XVEL(3) = 0.D0

               ELSE IF ( SYSNMS(SI) .EQ. CYLSYS ) THEN

                  XVEL(1) = 0.D0
                  XVEL(2) = 0.D0
                  XVEL(3) = STATE(6)

               ELSE IF ( SYSNMS(SI) .EQ. GEOSYS ) THEN

                  XVEL(1) = 0.D0
                  XVEL(2) = 0.D0
                  XVEL(3) = DP

               ELSE IF ( SYSNMS(SI) .EQ. PGRSYS ) THEN

                  XVEL(1) = 0.D0
                  XVEL(2) = 0.D0
                  XVEL(3) = DP

               ELSE

                  CALL SETMSG ( 'Bad coordinate system #' )
                  CALL ERRCH  ( '#', SYSNMS(SI)           )
                  CALL SIGERR ( 'SPICE(BUG)'              )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF

!
!              Set shape and spin parameters.
!
               RE    = 1.0D5
               F     = 0.5D0
               SENSE = -1

               CALL ZZGFCPRX ( STATE, SYSNMS(SI), RE, F, SENSE, CDSIGN )

               DO CI = 1, 3

                  QNAME = 'sign of d('
                  CALL SUFFIX ( CRDNMS(CI,SI), 0, QNAME )
                  CALL SUFFIX ( ')/dt',        0, QNAME )

                  IF ( XVEL(CI) .GT. 0.D0 ) THEN

                     CALL CHCKSI ( QNAME, CDSIGN(CI), '=',  1, 0, OK )

                  ELSE IF ( XVEL(CI) .LT. 0.D0 ) THEN

                     CALL CHCKSI ( QNAME, CDSIGN(CI), '=', -1, 0, OK )
                  ELSE
                     CALL CHCKSI ( QNAME, CDSIGN(CI), '=',  0, 0, OK )
                  END IF

               END DO
!
!              End of coordinate system loop.
!
            END DO
!
!           End of coordinate loop.
!

         END DO
!
!        End of derivative direction loop.
!
      END DO
!
!     End of Z-axis position loop.
!


!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

!*********************************************************************
!*
!*    Rectangular coordinate system cases
!*
!*********************************************************************

!
!     Since outputs for the rectangular coordinate system require
!     special handling by ZZGFCPRX, we test the routine's processing
!     for this case separately.
!
!     We try cases where each velocity component is negative, 0, or
!     positive.
!
      CALL VPACK ( 1.D0, 2.D0, 3.D0, STATE )

      DO I = 1, 3

         STATE(4) = DBLE(I-2)

         DO J = 1, 3

            STATE(5) = DBLE(J-2)

            DO K = 1, 3

               STATE(6) = DBLE(K-2)
!
!---- Case -------------------------------------------------------------
!
               TITLE = 'Velocity (# # #)'

               CALL REPMD ( TITLE, '#', STATE(4), 3, TITLE )
               CALL REPMD ( TITLE, '#', STATE(5), 3, TITLE )
               CALL REPMD ( TITLE, '#', STATE(6), 3, TITLE )

               CALL TCASE ( TITLE )

               RE    = 1.D0
               F     = 0.D0
               SENSE = 1

               CALL ZZGFCPRX ( STATE, RECSYS, RE, F, SENSE, CDSIGN )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSI ( 'X vel sign',    CDSIGN(1), '=', &
                             INT( STATE(4) ), 0,         OK  )
            END DO

         END DO

      END DO



!*********************************************************************
!*
!*    Non-rectangular coordinate system cases
!*
!*********************************************************************

!
!     We're going to loop over a variety of state vectors
!     and all coordinate system/coordinate combinations.
!     For each test, the derivative signs we find must match
!     those of the corresponding coordinate derivative.
!

!
!     DELTLA and DELTLO are, respectively the latitude and longitude
!     increments we use to create different direction vectors.
!
      DELTLA = PI()   / ( NLATSM - 1 )
      DELTLO = 2*PI() /   NLONSM


      DO NLON = 1, NLONSM

         DO NLAT = 1, NLATSM
!
!           Create the latitudinal coordinates of a position vector.
!
            LON = (NLON-1)*DELTLO

            LAT = PI()/2 - (NLAT-1)*DELTLA

!
!           Don't pick a singular latitude value.
!
            LAT = MIN ( LAT, PI()/2 - EPSLAT )
            LAT = MAX ( LAT, EPSLAT - PI()/2 )

!
!           Fill in the position portion of the state vector.
!
            CALL LATREC ( 1.D0, LON, LAT, STATE )

!
!           Loop over the velocity directions.
!
            DO NDLON = 1, NLONSM

               DO NDLAT = 1, NLATSM
!
!                 Create a velocity direction. Tweak the direction
!                 so it doesn't differ from the position direction
!                 only by round-off; this prevents situations where
!                 we get the wrong derivative sign due to noise.
!
                  DLON = (NDLON-1)*DELTLO          + TWEAK
                  DLAT = PI()/2 - (NDLAT-1)*DELTLA + TWEAK

!
!                 Fill in the position portion of the state vector.
!
                  CALL LATREC ( 1.D0, DLON, DLAT, STATE(4) )


                  DO SI = 1, NSYS
!
!---- Case -------------------------------------------------------------
!
                     TITLE = '#; lat #(deg); lon #(deg); ' &
                     //      'dlat #(deg); dlon #(deg)'

                     CALL REPMC ( TITLE, '#', SYSNMS(SI),     TITLE )
                     CALL REPMD ( TITLE, '#', DPR()*LAT,   3, TITLE )
                     CALL REPMD ( TITLE, '#', DPR()*LON,   3, TITLE )
                     CALL REPMD ( TITLE, '#', DPR()*DLAT,  3, TITLE )
                     CALL REPMD ( TITLE, '#', DPR()*DLON,  3, TITLE )

                     CALL TCASE ( TITLE )

!
!                    Set shape and spin parameters.
!
                     RE    = 1.0D5
                     F     = 0.5D0
                     SENSE = -1

!
!                    Look up the Jacobian matrix for the current system
!                    and state. Find the expected coordinate rates.
!
                     IF ( SYSNMS(SI) .EQ. RECSYS ) THEN

                        CALL IDENT ( JACOBI )

                     ELSE IF ( SYSNMS(SI) .EQ. LATSYS ) THEN

                        CALL DLATDR ( STATE(1), STATE(2), STATE(3), &
                                      JACOBI                       )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     ELSE IF ( SYSNMS(SI) .EQ. RADSYS ) THEN

                        CALL DLATDR ( STATE(1), STATE(2), STATE(3), &
                                      JACOBI                       )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     ELSE IF ( SYSNMS(SI) .EQ. SPHSYS ) THEN

                        CALL DSPHDR ( STATE(1), STATE(2), STATE(3), &
                                      JACOBI                       )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     ELSE IF ( SYSNMS(SI) .EQ. CYLSYS ) THEN

                        CALL DCYLDR ( STATE(1), STATE(2), STATE(3), &
                                      JACOBI                       )
                        CALL CHCKXC ( .FALSE., ' ', OK )


                     ELSE IF ( SYSNMS(SI) .EQ. GEOSYS ) THEN

                        CALL DGEODR ( STATE(1), STATE(2), STATE(3), &
                                      RE,       F,        JACOBI   )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     ELSE IF ( SYSNMS(SI) .EQ. PGRSYS ) THEN

                        CALL DPGRDR ( 'Mars', &
                                      STATE(1), STATE(2), STATE(3), &
                                      RE,       F,        JACOBI   )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     ELSE

                        CALL SETMSG ( 'Bad coordinate system #' )
                        CALL ERRCH  ( '#', SYSNMS(SI)           )
                        CALL SIGERR ( 'SPICE(BUG)'              )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     END IF


                     CALL MXV ( JACOBI, STATE(4), XVEL )


                     CALL ZZGFCPRX ( STATE, SYSNMS(SI), RE, F, &
                                     SENSE, CDSIGN             )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     DO CI = 1, 3

                        QNAME = 'sign of d('
                        CALL SUFFIX ( CRDNMS(CI,SI), 0, QNAME )
                        CALL SUFFIX ( ')/dt',        0, QNAME )

                        IF ( XVEL(CI) .GT. 0.D0 ) THEN

                           CALL CHCKSI ( QNAME, CDSIGN(CI), &
                                         '=',  1, 0, OK       )

                        ELSE IF ( XVEL(CI) .LT. 0.D0 ) THEN

                           CALL CHCKSI ( QNAME, CDSIGN(CI), &
                                         '=',  -1, 0, OK       )

                        ELSE

                           CALL CHCKSI ( QNAME, CDSIGN(CI), &
                                         '=',  0, 0, OK       )
                        END IF

                     END DO
!
!                    End of coordinate loop.
!
                  END DO
!
!                 End of system loop.
!
               END DO
!
!              End of latitude rate loop.
!
            END DO
!
!           End of longitude rate loop.
!
         END DO
!
!        End of latitude loop.
!
      END DO
!
!     End of longitude loop.
!

      CALL T_SUCCESS ( OK )
      RETURN
      END




