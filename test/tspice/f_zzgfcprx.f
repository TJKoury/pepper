C$Procedure      F_ZZGFCPRX ( Test coordinate derivative proxy routine )
  
      SUBROUTINE F_ZZGFCPRX ( OK )

C$ Abstract
C
C     Test the GF private coordinate derivative proxy routine ZZGFCPRX.
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
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

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
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the private SPICELIB GF
C     coordinate derivative proxy routine ZZGFCPRX.
C
C$ Examples
C
C     None.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 27-JUN-2008 (NJB)
C
C        Added new test cases:
C
C           - Handling of invalid coordinate system
C           - Handling of the rectangular system
C           - Handling of the DPSIGN==0 case where
C             the position is on the Z-axis
C           - Handling of the Z-velocity == 0 case
C             for cylindrical coordinates, where the
C             position is on the Z-axis
C           - Zero input velocity
C
C-    SPICELIB Version 1.0.0, 11-MAY-2008 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDOT

C     
C     Local parameters
C
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgfcprx.tpc' )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

C
C     Small delta value used for near 0 and pi 
C     latitudes.
C
      DOUBLE PRECISION      EPSLAT
      PARAMETER           ( EPSLAT = 1.D-1 )

      DOUBLE PRECISION      TWEAK
      PARAMETER           ( TWEAK  = 1.D-3 )

C
C     Number of recognized coordinate systems.
C
      INTEGER               NSYS
      PARAMETER           ( NSYS   = 7 )

C
C     Maximum length of a coordinate system name.
C
      INTEGER               SYSLEN
      PARAMETER           ( SYSLEN = 32 )
      
C
C     Maximum length of a coordinate name.
C
      INTEGER               CRDLEN
      PARAMETER           ( CRDLEN = 32 )
      
C
C     Numbers of latitude and longitude samples.
C 
      INTEGER               NLATSM
      PARAMETER           ( NLATSM = 5 )

      INTEGER               NLONSM
      PARAMETER           ( NLONSM = 8 )

C
C     Local Variables
C
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

C
C     Save everything.
C
      SAVE
 
C
C     Initial values 
C

C
C     Names of supported coordinate systems.
C
C     The Ith coordinate system in the array SYSNMS has coordinates
C     in the Ith row of the array CRDNMS. This association must be
C     preserved when this routine is updated.
C
      DATA                  SYSNMS /  RECSYS, 
     .                                LATSYS, 
     .                                RADSYS, 
     .                                SPHSYS, 
     .                                CYLSYS, 
     .                                GEOSYS,
     .                                PGRSYS  /

C
C     Names of coordinate triples for the supported coordinate
C     systems.
C
C     The order of the coordinate names in the Ith row of this array
C     matches the order of the outputs of the corresponding
C     SPICELIB routine REC*, which maps a Cartesian vector to
C     the Ith coordinate system in the array SYSNMS. Again, this
C     order must be preserved.
C     
      DATA                  CRDNMS /  XCRD,     YCRD,     ZCRD,
     .                                RADCRD,   LONCRD,   LATCRD,
     .                                RNGCRD,   RACRD,    DECCRD,
     .                                RADCRD,   CLTCRD,   LONCRD,
     .                                RADCRD,   LONCRD,   ZCRD,
     .                                LONCRD,   LATCRD,   ALTCRD,
     .                                LONCRD,   LATCRD,   ALTCRD  /



C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFCPRX' )




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Setup: create and load PCK file.' )
C
C     Load a PCK.
C     
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad coordinate system' )

C
C     Set shape and spin parameters.
C
      RE    = 1.0D5
      F     = 0.5D0
      SENSE = -1 

      CALL FILLD ( 1.D0, 6, STATE )

      CALL ZZGFCPRX ( STATE, 'GAUSSIAN', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad longitude sense' )

C
C     Set shape and spin parameters.
C
      RE    = 1.0D5
      F     = 0.5D0
      SENSE = -2

      CALL FILLD ( 1.D0, 6, STATE )

      CALL ZZGFCPRX ( STATE, 'PLANETOGRAPHIC', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad equatorial radius' )

C
C     Set shape and spin parameters.
C
      RE    = -1.0D5
      F     = 0.5D0
      SENSE = 1

      CALL FILLD ( 1.D0, 6, STATE )

      CALL ZZGFCPRX ( STATE, 'PLANETOGRAPHIC', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL ZZGFCPRX ( STATE, 'GEODETIC', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad flattening coefficient' )

C
C     Set shape and spin parameters.
C
      RE    = 1.0D5
      F     = 1.0D6
      SENSE = 1

      CALL FILLD ( 1.D0, 6, STATE )

      CALL ZZGFCPRX ( STATE, 'PLANETOGRAPHIC', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL ZZGFCPRX ( STATE, 'GEODETIC', RE, F, SENSE, CDSIGN )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C*********************************************************************
C*
C*    Non-error Exceptional cases
C*
C*********************************************************************



C
C     Test handling of zero velocity vector.
C
      CALL TCASE ( 'Input velocity is zero; position is '
     .//           'off Z-axis.'                          )

      CALL VPACK  ( 1.D0, 1.D0, 1.D0, STATE )
      CALL CLEARD ( 3, STATE(4) )

      DO SI = 1, NSYS
C
C---- Case -------------------------------------------------------------
C
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



C     Test handling of zero velocity vector.
C
      CALL TCASE ( 'Input velocity is zero; position is '
     .//           'on Z-axis.'                          )

      CALL VPACK  ( 0.D0, 0.D0, 1.D0, STATE )
      CALL CLEARD ( 3, STATE(4) )

      DO SI = 1, NSYS
C
C---- Case -------------------------------------------------------------
C
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
 


C
C     Test results for positions on the Z-axis, with velocity
C     orthogonal to Z. Applies only to the cylindrical system.
C
C
C---- Case -------------------------------------------------------------
C
      TITLE = 'velocity orthogonal to Z; system is #'

      CALL REPMC ( TITLE, '#', CYLSYS, TITLE )

      CALL TCASE ( TITLE )

      CALL VPACK  ( 0.D0, 0.D0, 1.D0, STATE    )
      CALL VPACK  ( 1.D0, 0.D0, 0.D0, STATE(4) )


      CALL ZZGFCPRX ( STATE, CYLSYS, 1.D0, 0.D0, 1, CDSIGN )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      DO CI = 1, 3
C
C        Note: the index of the cylindrical system is 5.
C
         QNAME = 'sign of d('
         CALL SUFFIX ( CRDNMS(CI,5), 0, QNAME )
         CALL SUFFIX ( ')/dt',       0, QNAME )

         CALL CHCKSI ( QNAME, CDSIGN(CI), '=',  0, 0, OK )

      END DO


C
C     Test results for range or altitude coordinates for positions on
C     the Z-axis, with velocity orthogonal to Z. Applies to all but
C     rectangular systems.
C
C     The rectangular system has index 1, so start at index 2.
C
      DO SI = 2, NSYS
C
C---- Case -------------------------------------------------------------
C
         CRDSYS = SYSNMS(SI)

         TITLE = 'velocity orthogonal to Z; system is #'

         CALL REPMC ( TITLE, '#', CRDSYS, TITLE )

         CALL TCASE ( TITLE )

         CALL VPACK  ( 0.D0, 0.D0, 1.D0, STATE    )
         CALL VPACK  ( 1.D0, 0.D0, 0.D0, STATE(4) )


         CALL ZZGFCPRX ( STATE, CYLSYS, 1.D0, 0.D0, 1, CDSIGN )
         CALL CHCKXC   ( .FALSE., ' ', OK )

C
C        Set CI to the index of the radius or altitude coordinate
C        of the current system.
C
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


C
C---- Case -------------------------------------------------------------
C

C
C     Test results for positions on the Z-axis.
C
      DO NLAT = 1, 3
C
C        Generate Z values corresponding to 
C      
C           +1, 0, -1
C
C        positions, respectively.
C
         STATE(1) = 0.D0
         STATE(2) = 0.D0
         STATE(3) = 2.D0 - DBLE(NLAT)


         DO NDLAT = 1, 2 
C
C           Generate latitude values corresponding to 
C      
C              +Pi/4, -Pi/4
C
C           positions, respectively.
C
            DLAT = (3 - 2*NDLAT) * PI()/4.D0
            DLON = PI()/4.D0

            CALL LATREC ( 1.D0, DLON, DLAT, STATE(4) )
            
            DP = VDOT ( STATE, STATE(4) )
C
C           Check the coordinate derivative signs for
C           each coordinate system.
C           
            DO SI = 1, NSYS
C
C---- Case -------------------------------------------------------------
C
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

C
C              Set shape and spin parameters.
C
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
C
C              End of coordinate system loop.
C
            END DO
C
C           End of coordinate loop.
C

         END DO
C
C        End of derivative direction loop.
C
      END DO
C
C     End of Z-axis position loop.
C


C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************

C*********************************************************************
C*
C*    Rectangular coordinate system cases
C*
C*********************************************************************

C
C     Since outputs for the rectangular coordinate system require
C     special handling by ZZGFCPRX, we test the routine's processing
C     for this case separately.
C
C     We try cases where each velocity component is negative, 0, or
C     positive.
C
      CALL VPACK ( 1.D0, 2.D0, 3.D0, STATE )

      DO I = 1, 3

         STATE(4) = DBLE(I-2)

         DO J = 1, 3 

            STATE(5) = DBLE(J-2)

            DO K = 1, 3

               STATE(6) = DBLE(K-2)
C
C---- Case -------------------------------------------------------------
C
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

               CALL CHCKSI ( 'X vel sign',    CDSIGN(1), '=', 
     .                       INT( STATE(4) ), 0,         OK  ) 
            END DO

         END DO

      END DO



C*********************************************************************
C*
C*    Non-rectangular coordinate system cases
C*
C*********************************************************************

C
C     We're going to loop over a variety of state vectors
C     and all coordinate system/coordinate combinations.
C     For each test, the derivative signs we find must match
C     those of the corresponding coordinate derivative.
C

C
C     DELTLA and DELTLO are, respectively the latitude and longitude
C     increments we use to create different direction vectors.
C
      DELTLA = PI()   / ( NLATSM - 1 )
      DELTLO = 2*PI() /   NLONSM


      DO NLON = 1, NLONSM

         DO NLAT = 1, NLATSM
C
C           Create the latitudinal coordinates of a position vector.
C
            LON = (NLON-1)*DELTLO

            LAT = PI()/2 - (NLAT-1)*DELTLA

C
C           Don't pick a singular latitude value.
C
            LAT = MIN ( LAT, PI()/2 - EPSLAT )
            LAT = MAX ( LAT, EPSLAT - PI()/2 )

C
C           Fill in the position portion of the state vector.
C
            CALL LATREC ( 1.D0, LON, LAT, STATE )

C
C           Loop over the velocity directions.
C
            DO NDLON = 1, NLONSM

               DO NDLAT = 1, NLATSM
C
C                 Create a velocity direction. Tweak the direction
C                 so it doesn't differ from the position direction
C                 only by round-off; this prevents situations where
C                 we get the wrong derivative sign due to noise.
C
                  DLON = (NDLON-1)*DELTLO          + TWEAK
                  DLAT = PI()/2 - (NDLAT-1)*DELTLA + TWEAK

C
C                 Fill in the position portion of the state vector.
C
                  CALL LATREC ( 1.D0, DLON, DLAT, STATE(4) )


                  DO SI = 1, NSYS
C
C---- Case -------------------------------------------------------------
C
                     TITLE = '#; lat #(deg); lon #(deg); '
     .               //      'dlat #(deg); dlon #(deg)'

                     CALL REPMC ( TITLE, '#', SYSNMS(SI),     TITLE )
                     CALL REPMD ( TITLE, '#', DPR()*LAT,   3, TITLE )
                     CALL REPMD ( TITLE, '#', DPR()*LON,   3, TITLE )
                     CALL REPMD ( TITLE, '#', DPR()*DLAT,  3, TITLE )
                     CALL REPMD ( TITLE, '#', DPR()*DLON,  3, TITLE )

                     CALL TCASE ( TITLE )

C
C                    Set shape and spin parameters.
C
                     RE    = 1.0D5
                     F     = 0.5D0
                     SENSE = -1

C
C                    Look up the Jacobian matrix for the current system
C                    and state. Find the expected coordinate rates.
C                     
                     IF ( SYSNMS(SI) .EQ. RECSYS ) THEN

                        CALL IDENT ( JACOBI )

                     ELSE IF ( SYSNMS(SI) .EQ. LATSYS ) THEN

                        CALL DLATDR ( STATE(1), STATE(2), STATE(3), 
     .                                JACOBI                       )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     ELSE IF ( SYSNMS(SI) .EQ. RADSYS ) THEN

                        CALL DLATDR ( STATE(1), STATE(2), STATE(3), 
     .                                JACOBI                       )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     ELSE IF ( SYSNMS(SI) .EQ. SPHSYS ) THEN

                        CALL DSPHDR ( STATE(1), STATE(2), STATE(3), 
     .                                JACOBI                       )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     ELSE IF ( SYSNMS(SI) .EQ. CYLSYS ) THEN

                        CALL DCYLDR ( STATE(1), STATE(2), STATE(3), 
     .                                JACOBI                       )
                        CALL CHCKXC ( .FALSE., ' ', OK )


                     ELSE IF ( SYSNMS(SI) .EQ. GEOSYS ) THEN

                        CALL DGEODR ( STATE(1), STATE(2), STATE(3), 
     .                                RE,       F,        JACOBI   )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     ELSE IF ( SYSNMS(SI) .EQ. PGRSYS ) THEN

                        CALL DPGRDR ( 'Mars',
     .                                STATE(1), STATE(2), STATE(3), 
     .                                RE,       F,        JACOBI   )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     ELSE
                        
                        CALL SETMSG ( 'Bad coordinate system #' )
                        CALL ERRCH  ( '#', SYSNMS(SI)           )
                        CALL SIGERR ( 'SPICE(BUG)'              )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     END IF


                     CALL MXV ( JACOBI, STATE(4), XVEL )


                     CALL ZZGFCPRX ( STATE, SYSNMS(SI), RE, F, 
     .                               SENSE, CDSIGN             )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     DO CI = 1, 3

                        QNAME = 'sign of d('
                        CALL SUFFIX ( CRDNMS(CI,SI), 0, QNAME )
                        CALL SUFFIX ( ')/dt',        0, QNAME )

                        IF ( XVEL(CI) .GT. 0.D0 ) THEN

                           CALL CHCKSI ( QNAME, CDSIGN(CI),
     .                                   '=',  1, 0, OK       )

                        ELSE IF ( XVEL(CI) .LT. 0.D0 ) THEN

                           CALL CHCKSI ( QNAME, CDSIGN(CI),
     .                                   '=',  -1, 0, OK       )

                        ELSE

                           CALL CHCKSI ( QNAME, CDSIGN(CI),
     .                                   '=',  0, 0, OK       )
                        END IF

                     END DO
C
C                    End of coordinate loop.
C
                  END DO
C
C                 End of system loop.
C
               END DO
C
C              End of latitude rate loop.
C
            END DO
C
C           End of longitude rate loop.
C           
         END DO
C
C        End of latitude loop.
C
      END DO
C
C     End of longitude loop.
C

      CALL T_SUCCESS ( OK )
      RETURN
      END




