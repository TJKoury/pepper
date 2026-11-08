!$Procedure ZZDSPC ( SGP4 deep space routine )

      SUBROUTINE ZZDSPC( IREZ,  D2201, D2211, D3210,   D3222, &
                         D4410, D4422, D5220, D5232,   D5421, D5433, &
                         DEDT,  DEL1,  DEL2,  DEL3,    DIDT,  DMDT, &
                         DNODT, DOMDT, ARGPO, ARGPDOT, T,     TC, &
                         GSTO,  XFACT, XLAMO, NO,      ATIME, ECCM, &
                         ARGPM, INCLM, XLI,   MM,      XNI,   NODEM, &
                         DNDT,  XN  )

! $ Abstract
!
!     This subroutine provides deep space contributions to mean
!     elements for perturbing third body. These effects have been
!     averaged over one revolution of the sun and moon. For earth
!     resonance effects, the effects have been averaged over NO
!     revolutions of the satellite.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      IMPLICIT NONE

      INTEGER                  IREZ

      DOUBLE PRECISION         D2201
      DOUBLE PRECISION         D2211
      DOUBLE PRECISION         D3210
      DOUBLE PRECISION         D3222
      DOUBLE PRECISION         D4410
      DOUBLE PRECISION         D4422
      DOUBLE PRECISION         D5220
      DOUBLE PRECISION         D5232
      DOUBLE PRECISION         D5421
      DOUBLE PRECISION         D5433
      DOUBLE PRECISION         DEDT
      DOUBLE PRECISION         DEL1
      DOUBLE PRECISION         DEL2
      DOUBLE PRECISION         DEL3
      DOUBLE PRECISION         DIDT
      DOUBLE PRECISION         DMDT
      DOUBLE PRECISION         DNODT
      DOUBLE PRECISION         DOMDT
      DOUBLE PRECISION         ARGPO
      DOUBLE PRECISION         ARGPDOT
      DOUBLE PRECISION         T
      DOUBLE PRECISION         TC
      DOUBLE PRECISION         GSTO
      DOUBLE PRECISION         XFACT
      DOUBLE PRECISION         XLAMO
      DOUBLE PRECISION         NO
      DOUBLE PRECISION         ATIME
      DOUBLE PRECISION         ECCM
      DOUBLE PRECISION         ARGPM
      DOUBLE PRECISION         INCLM
      DOUBLE PRECISION         XLI
      DOUBLE PRECISION         MM
      DOUBLE PRECISION         XNI
      DOUBLE PRECISION         NODEM
      DOUBLE PRECISION         DNDT
      DOUBLE PRECISION         XN

! $ Brief_I/O
!
!    See Detailed_input and Detailed_Output.
!
! $ Detailed_Input
!
!     D2201      D coefficients
!
!     D2211         ...
!
!     D3210         ...
!
!     D3222         ...
!
!     D4410         ...
!
!     D4422         ...
!
!     D5220         ...
!
!     D5232         ...
!
!     D5421         ...
!
!     D5433         ...
!
!     DEDT       Internal SGD4 parameter.
!
!     DEL1       Internal SGD4 parameter.
!
!     DEL2       Internal SGD4 parameter.
!
!     DEL3       Internal SGD4 parameter.
!
!     DIDT       Internal SGD4 parameter.
!
!     DMDT       Internal SGD4 parameter.
!
!     DNODT      Internal SGD4 parameter.
!
!     DOMDT      Internal SGD4 parameter.
!
!     IREZ       Flag for resonance: 0-none, 1-one day, 2-half day
!.
!     ARGPO      Argument of perigee
!
!     ARGPDOT    Argument of perigee dot (rate)
!
!     T          Time of evaluation.
!
!     TC         Internal SGD4 parameter.
!
!     GSTO       Greenwich Sidereal Time
!
!     XFACT      Internal SGD4 parameter.
!
!     XLAMO      Internal SGD4 parameter.
!
!     NO         Mean motion.
!
!     ATIME      Internal SGD4 parameter.
!
!     EM         Eccentricity.
!
!     FT         Internal SGD4 parameter.
!
!     ARGPM      Argument of perigee
!
!     INCLM      Inclination.
!
!     XLI        Internal SGD4 parameter.
!
!     MM         Mean anomaly.
!
!     XNI        Mean motion.
!
!     NODEM      Right ascension of ascending node
!
! $ Detailed_Output
!
!     ATIME      Internal SGD4 parameter.
!
!     EM         Calculated mean eccentricity.
!
!     ARGPM      Calculated mean argument of perigee.
!
!     INCLM      Calculated mean inclination.
!
!     XLI        Internal SGD4 parameter.
!
!     MM         Calculated mean anomaly.
!
!     XNI        Calculated  mean motion.
!
!     NODEM      Calculated mean right ascension of
!                ascending node.
!
!     DNDT       Value XN-NO.
!
!     NM         Calculated mean motion.
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
!     This routine is based on the DSPACE code by David Vallado
!     corresponding to "Revisiting Spacetrack Report #3" [4].
!     The intent is to maintain the original Vallado algorithm,
!     changing code only to meet NAIF format standards and to
!     integrate with SPICELIB.
!
!        Capitalize all variables.
!
!        ENDIF replaced with END IF.
!
!        ENDDO replaced with END DO.
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
!   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
!       Propagation of the NORAD Element Sets." Spacetrack Report #3.
!       U.S. Air Force: Aerospace Defense Command.
!
!   [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
!       of Space Command Element Sets." Space Command,
!       U. S. Air Force, CO.
!
!   [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
!       History of Analytical Orbit Modeling in the U. S. Space
!       Surveillance System. Journal of Guidance, Control, and
!       Dynamics. 27(2):174-185.
!
!   [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
!       and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
!       AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
!       Specialist Conference, August 21-24, 2006. Keystone, CO.
!
! $ Author_and_Institution
!
!     David Vallado   (AGI)
!     E. D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    SPICELIB Version 1.0.0, OCT-14-2014 (EDW)
!
!        Based on routine DPSPACE, 28-JUN-2005, Vallado 2006 [4].
!
!-&

! $ Index_Entries
!
!   SGP4
!
!-&

!
!     Local Variables
!

      DOUBLE PRECISION         DELT
      DOUBLE PRECISION         FASX2
      DOUBLE PRECISION         FASX4
      DOUBLE PRECISION         FASX6
      DOUBLE PRECISION         FT
      DOUBLE PRECISION         G22
      DOUBLE PRECISION         G32
      DOUBLE PRECISION         G44
      DOUBLE PRECISION         G52
      DOUBLE PRECISION         G54
      DOUBLE PRECISION         RPTIM
      DOUBLE PRECISION         STEP2
      DOUBLE PRECISION         STEPN
      DOUBLE PRECISION         STEPP
      DOUBLE PRECISION         THETA
      DOUBLE PRECISION         X2LI
      DOUBLE PRECISION         X2OMI
      DOUBLE PRECISION         XL
      DOUBLE PRECISION         XLDOT
      DOUBLE PRECISION         XNDDT
      DOUBLE PRECISION         XNDT
      DOUBLE PRECISION         XOMI

      INTEGER                  IRET
      INTEGER                  IRETN


!
!     SPICELIB routines.
!
      DOUBLE PRECISION         TWOPI
      LOGICAL                  RETURN

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDSPC' )

!
!     Constants
!
      FASX2 = 0.13130908D0
      FASX4 = 2.8843198D0
      FASX6 = 0.37448087D0
      G22   = 5.7686396D0
      G32   = 0.95240898D0
      G44   = 1.8014998D0
      G52   = 1.0508330D0
      G54   = 4.4108898D0
      RPTIM = 4.37526908801129966D-3
      STEPP =    720.0D0
      STEPN =   -720.0D0
      STEP2 = 259200.0D0

!
!     Calculate deep space resonance effects.
!
      DNDT  = 0.0D0
      THETA = DMOD(GSTO + TC*RPTIM,TWOPI())
      ECCM  = ECCM + DEDT*T

      INCLM = INCLM + DIDT*T
      ARGPM = ARGPM + DOMDT*T
      NODEM = NODEM + DNODT*T
      MM    = MM + DMDT*T

!
!   sgp4fix for negative inclinations
!   the following if statement should be commented out
!
!        IF( INCLM .LT. 0.0D0) THEN
!            INCLM  = -INCLM
!            ARGPM  = ARGPM-PI
!            NODEM  = NODEM+PI
!        END IF
!

!
!     sgp4fix for propagator problems
!
!     The following integration works for negative time steps and
!     periods. The specific changes are unknown because the original
!     code was so convoluted
!
!     sgp4fix Take out atime = 0.0 and fix for faster operation
!

!
!     Just in case - should be set in loops if used.
!
      FT    = 0.0D0

      IF (IREZ .ne. 0) THEN

!
!     UPDATE RESONANCES : NUMERICAL (EULER-MACLAURIN) INTEGRATION
!
!     EPOCH RESTART
!

!
!        sgp4fix streamline check
!
         IF ( (    ATIME .EQ. 0.0D0) .OR. &
              (T * ATIME .LE. 0.0D0) .OR. &
              (  DABS(T) .LT. DABS(ATIME))  ) THEN
            ATIME  = 0.0D0
            XNI    = NO
            XLI    = XLAMO
         END IF

!
!        sgp4fix move check outside loop
!
         IF (T .GT. 0.0D0) THEN
            DELT = STEPP
         ELSE
            DELT = STEPN
         END IF

!
!        ADDED FOR DO LOOP
!
         IRETN = 381

!
!        ADDED FOR LOOP
!
         IRET  =   0

         DO WHILE (IRETN.EQ.381)

!
!           DOT TERMS CALCULATED
!
!           NEAR - SYNCHRONOUS RESONANCE TERMS
!
            IF (IREZ .NE. 2) THEN

               XNDT  = DEL1*DSIN(XLI-FASX2) + &
                        DEL2*DSIN(2.0D0*(XLI-FASX4)) + &
                        DEL3*DSIN(3.0D0*(XLI-FASX6))
               XLDOT = XNI + XFACT
               XNDDT = DEL1*DCOS(XLI-FASX2) + &
                  2.0D0*DEL2*DCOS(2.0D0*(XLI-FASX4)) + &
                  3.0D0*DEL3*DCOS(3.0D0*(XLI-FASX6))
               XNDDT = XNDDT*XLDOT

            ELSE

!
!              NEAR - HALF-DAY RESONANCE TERMS
!

               XOMI = ARGPO + ARGPDOT*ATIME
               X2OMI= XOMI + XOMI
               X2LI = XLI + XLI
               XNDT = D2201*DSIN(X2OMI+XLI-G22) + &
                       D2211*DSIN(XLI-G22) + &
                       D3210*DSIN( XOMI+XLI-G32) + &
                       D3222*DSIN(-XOMI+XLI-G32) + &
                       D4410*DSIN(X2OMI+X2LI-G44)+ &
                       D4422*DSIN(X2LI-G44)+ &
                       D5220*DSIN( XOMI+XLI-G52) + &
                       D5232*DSIN(-XOMI+XLI-G52) + &
                       D5421*DSIN( XOMI+X2LI-G54)+ &
                       D5433*DSIN(-XOMI+X2LI-G54)
               XLDOT = XNI+XFACT
               XNDDT = D2201*DCOS(X2OMI+XLI-G22) + &
                        D2211*DCOS(XLI-G22)+ &
                        D3210*DCOS( XOMI+XLI-G32) + &
                        D3222*DCOS(-XOMI+XLI-G32) + &
                        D5220*DCOS( XOMI+XLI-G52) + &
                        D5232*DCOS(-XOMI+XLI-G52) + &
                        2.0D0*(D4410*DCOS(X2OMI+X2LI-G44) + &
                        D4422*DCOS(X2LI-G44) + &
                        D5421*DCOS( XOMI+X2LI-G54) + &
                        D5433*DCOS(-XOMI+X2LI-G54))
               XNDDT = XNDDT*XLDOT

            END IF

!
!           INTEGRATOR
!
!           sgp4fix move end checks to end of routine
!

            IF (DABS(T-ATIME).GE.STEPP) THEN
               IRET  = 0
               IRETN = 381
            ELSE
               FT    = T-ATIME
               IRETN = 0
            END IF

            IF (IRETN.EQ.381) THEN
               XLI   = XLI + XLDOT*DELT + XNDT*STEP2
               XNI   = XNI + XNDT*DELT + XNDDT*STEP2
               ATIME = ATIME + DELT
            END IF

         END DO

         XN = XNI + XNDT*FT  + XNDDT*FT*FT*0.5D0
         XL = XLI + XLDOT*FT + XNDT*FT*FT*0.5D0

         IF(IREZ .NE. 1) THEN
            MM   = XL-2.0D0*NODEM+2.0D0*THETA
            DNDT = XN-NO
         ELSE
            MM   = XL-NODEM-ARGPM+THETA
            DNDT = XN-NO
         END IF

         XN = NO + DNDT

      END IF

      CALL CHKOUT ( 'ZZDSPC' )

      RETURN
      END
