!$Procedure ZZDSPC ( SGP4 deep space routine )

      subroutine zzdspc( irez,  d2201, d2211, d3210,   d3222, &
                         d4410, d4422, d5220, d5232,   d5421, d5433, &
                         dedt,  del1,  del2,  del3,    didt,  dmdt, &
                         dnodt, domdt, argpo, argpdot, t,     tc, &
                         gsto,  xfact, xlamo, no,      atime, eccm, &
                         argpm, inclm, xli,   mm,      xni,   nodem, &
                         dndt,  xn  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     This subroutine provides deep space contributions to mean
!     elements for perturbing third body. These effects have been
!     averaged over one revolution of the sun and moon. For earth
!     resonance effects, the effects have been averaged over NO
!     revolutions of the satellite.
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

      implicit none

      integer(ip)              irez

      real(wp)                 d2201
      real(wp)                 d2211
      real(wp)                 d3210
      real(wp)                 d3222
      real(wp)                 d4410
      real(wp)                 d4422
      real(wp)                 d5220
      real(wp)                 d5232
      real(wp)                 d5421
      real(wp)                 d5433
      real(wp)                 dedt
      real(wp)                 del1
      real(wp)                 del2
      real(wp)                 del3
      real(wp)                 didt
      real(wp)                 dmdt
      real(wp)                 dnodt
      real(wp)                 domdt
      real(wp)                 argpo
      real(wp)                 argpdot
      real(wp)                 t
      real(wp)                 tc
      real(wp)                 gsto
      real(wp)                 xfact
      real(wp)                 xlamo
      real(wp)                 no
      real(wp)                 atime
      real(wp)                 eccm
      real(wp)                 argpm
      real(wp)                 inclm
      real(wp)                 xli
      real(wp)                 mm
      real(wp)                 xni
      real(wp)                 nodem
      real(wp)                 dndt
      real(wp)                 xn

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

      real(wp)                 delt
      real(wp)                 fasx2
      real(wp)                 fasx4
      real(wp)                 fasx6
      real(wp)                 ft
      real(wp)                 g22
      real(wp)                 g32
      real(wp)                 g44
      real(wp)                 g52
      real(wp)                 g54
      real(wp)                 rptim
      real(wp)                 step2
      real(wp)                 stepn
      real(wp)                 stepp
      real(wp)                 theta
      real(wp)                 x2li
      real(wp)                 x2omi
      real(wp)                 xl
      real(wp)                 xldot
      real(wp)                 xnddt
      real(wp)                 xndt
      real(wp)                 xomi

      integer(ip)              iret
      integer(ip)              iretn

!
!     SPICELIB routines.
!
      real(wp)                 twopi
      logical                  return

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Constants
!
      fasx2 = 0.13130908_wp
      fasx4 = 2.8843198_wp
      fasx6 = 0.37448087_wp
      g22   = 5.7686396_wp
      g32   = 0.95240898_wp
      g44   = 1.8014998_wp
      g52   = 1.0508330_wp
      g54   = 4.4108898_wp
      rptim = 4.37526908801129966e-3_wp
      stepp =    720.0_wp
      stepn =   -720.0_wp
      step2 = 259200.0_wp

!
!     Calculate deep space resonance effects.
!
      dndt  = 0.0_wp
      theta = dmod(gsto + tc*rptim,twopi())
      eccm  = eccm + dedt*t

      inclm = inclm + didt*t
      argpm = argpm + domdt*t
      nodem = nodem + dnodt*t
      mm    = mm + dmdt*t

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
      ft    = 0.0_wp

      if (irez /= 0) then

!
!     UPDATE RESONANCES : NUMERICAL (EULER-MACLAURIN) INTEGRATION
!
!     EPOCH RESTART
!

!
!        sgp4fix streamline check
!
         if ( (    atime == 0.0_wp) .or. &
              (t * atime <= 0.0_wp) .or. &
              (  dabs(t) < dabs(atime))  ) then
            atime  = 0.0_wp
            xni    = no
            xli    = xlamo
         end if

!
!        sgp4fix move check outside loop
!
         if (t > 0.0_wp) then
            delt = stepp
         else
            delt = stepn
         end if

!
!        ADDED FOR DO LOOP
!
         iretn = 381

!
!        ADDED FOR LOOP
!
         iret  =   0

         do while (iretn==381)

!
!           DOT TERMS CALCULATED
!
!           NEAR - SYNCHRONOUS RESONANCE TERMS
!
            if (irez /= 2) then

               xndt  = del1*dsin(xli-fasx2) + &
                        del2*dsin(2.0_wp*(xli-fasx4)) + &
                        del3*dsin(3.0_wp*(xli-fasx6))
               xldot = xni + xfact
               xnddt = del1*dcos(xli-fasx2) + &
                  2.0_wp*del2*dcos(2.0_wp*(xli-fasx4)) + &
                  3.0_wp*del3*dcos(3.0_wp*(xli-fasx6))
               xnddt = xnddt*xldot

            else

!
!              NEAR - HALF-DAY RESONANCE TERMS
!

               xomi = argpo + argpdot*atime
               x2omi= xomi + xomi
               x2li = xli + xli
               xndt = d2201*dsin(x2omi+xli-g22) + &
                       d2211*dsin(xli-g22) + &
                       d3210*dsin( xomi+xli-g32) + &
                       d3222*dsin(-xomi+xli-g32) + &
                       d4410*dsin(x2omi+x2li-g44)+ &
                       d4422*dsin(x2li-g44)+ &
                       d5220*dsin( xomi+xli-g52) + &
                       d5232*dsin(-xomi+xli-g52) + &
                       d5421*dsin( xomi+x2li-g54)+ &
                       d5433*dsin(-xomi+x2li-g54)
               xldot = xni+xfact
               xnddt = d2201*dcos(x2omi+xli-g22) + &
                        d2211*dcos(xli-g22)+ &
                        d3210*dcos( xomi+xli-g32) + &
                        d3222*dcos(-xomi+xli-g32) + &
                        d5220*dcos( xomi+xli-g52) + &
                        d5232*dcos(-xomi+xli-g52) + &
                        2.0_wp*(d4410*dcos(x2omi+x2li-g44) + &
                        d4422*dcos(x2li-g44) + &
                        d5421*dcos( xomi+x2li-g54) + &
                        d5433*dcos(-xomi+x2li-g54))
               xnddt = xnddt*xldot

            end if

!
!           INTEGRATOR
!
!           sgp4fix move end checks to end of routine
!

            if (dabs(t-atime)>=stepp) then
               iret  = 0
               iretn = 381
            else
               ft    = t-atime
               iretn = 0
            end if

            if (iretn==381) then
               xli   = xli + xldot*delt + xndt*step2
               xni   = xni + xndt*delt + xnddt*step2
               atime = atime + delt
            end if

         end do

         xn = xni + xndt*ft  + xnddt*ft*ft*0.5_wp
         xl = xli + xldot*ft + xndt*ft*ft*0.5_wp

         if(irez /= 1) then
            mm   = xl-2.0_wp*nodem+2.0_wp*theta
            dndt = xn-no
         else
            mm   = xl-nodem-argpm+theta
            dndt = xn-no
         end if

         xn = no + dndt

      end if

      return
      end
