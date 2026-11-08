!$Procedure ZZDSPC ( SGP4 deep space routine )

      subroutine zzdspc( irez,  d2201, d2211, d3210,   d3222, &
                         d4410, d4422, d5220, d5232,   d5421, d5433, &
                         dedt,  del1,  del2,  del3,    didt,  dmdt, &
                         dnodt, domdt, argpo, argpdot, t,     tc, &
                         gsto,  xfact, xlamo, no,      atime, eccm, &
                         argpm, inclm, xli,   mm,      xni,   nodem, &
                         dndt,  xn  )

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

      integer                  irez

      double precision         d2201
      double precision         d2211
      double precision         d3210
      double precision         d3222
      double precision         d4410
      double precision         d4422
      double precision         d5220
      double precision         d5232
      double precision         d5421
      double precision         d5433
      double precision         dedt
      double precision         del1
      double precision         del2
      double precision         del3
      double precision         didt
      double precision         dmdt
      double precision         dnodt
      double precision         domdt
      double precision         argpo
      double precision         argpdot
      double precision         t
      double precision         tc
      double precision         gsto
      double precision         xfact
      double precision         xlamo
      double precision         no
      double precision         atime
      double precision         eccm
      double precision         argpm
      double precision         inclm
      double precision         xli
      double precision         mm
      double precision         xni
      double precision         nodem
      double precision         dndt
      double precision         xn

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

      double precision         delt
      double precision         fasx2
      double precision         fasx4
      double precision         fasx6
      double precision         ft
      double precision         g22
      double precision         g32
      double precision         g44
      double precision         g52
      double precision         g54
      double precision         rptim
      double precision         step2
      double precision         stepn
      double precision         stepp
      double precision         theta
      double precision         x2li
      double precision         x2omi
      double precision         xl
      double precision         xldot
      double precision         xnddt
      double precision         xndt
      double precision         xomi

      integer                  iret
      integer                  iretn


!
!     SPICELIB routines.
!
      double precision         twopi
      logical                  return

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'ZZDSPC' )

!
!     Constants
!
      fasx2 = 0.13130908d0
      fasx4 = 2.8843198d0
      fasx6 = 0.37448087d0
      g22   = 5.7686396d0
      g32   = 0.95240898d0
      g44   = 1.8014998d0
      g52   = 1.0508330d0
      g54   = 4.4108898d0
      rptim = 4.37526908801129966d-3
      stepp =    720.0d0
      stepn =   -720.0d0
      step2 = 259200.0d0

!
!     Calculate deep space resonance effects.
!
      dndt  = 0.0d0
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
      ft    = 0.0d0

      if (irez .ne. 0) then

!
!     UPDATE RESONANCES : NUMERICAL (EULER-MACLAURIN) INTEGRATION
!
!     EPOCH RESTART
!

!
!        sgp4fix streamline check
!
         if ( (    atime .eq. 0.0d0) .or. &
              (t * atime .le. 0.0d0) .or. &
              (  dabs(t) .lt. dabs(atime))  ) then
            atime  = 0.0d0
            xni    = no
            xli    = xlamo
         end if

!
!        sgp4fix move check outside loop
!
         if (t .gt. 0.0d0) then
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

         do while (iretn.eq.381)

!
!           DOT TERMS CALCULATED
!
!           NEAR - SYNCHRONOUS RESONANCE TERMS
!
            if (irez .ne. 2) then

               xndt  = del1*dsin(xli-fasx2) + &
                        del2*dsin(2.0d0*(xli-fasx4)) + &
                        del3*dsin(3.0d0*(xli-fasx6))
               xldot = xni + xfact
               xnddt = del1*dcos(xli-fasx2) + &
                  2.0d0*del2*dcos(2.0d0*(xli-fasx4)) + &
                  3.0d0*del3*dcos(3.0d0*(xli-fasx6))
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
                        2.0d0*(d4410*dcos(x2omi+x2li-g44) + &
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

            if (dabs(t-atime).ge.stepp) then
               iret  = 0
               iretn = 381
            else
               ft    = t-atime
               iretn = 0
            end if

            if (iretn.eq.381) then
               xli   = xli + xldot*delt + xndt*step2
               xni   = xni + xndt*delt + xnddt*step2
               atime = atime + delt
            end if

         end do

         xn = xni + xndt*ft  + xnddt*ft*ft*0.5d0
         xl = xli + xldot*ft + xndt*ft*ft*0.5d0

         if(irez .ne. 1) then
            mm   = xl-2.0d0*nodem+2.0d0*theta
            dndt = xn-no
         else
            mm   = xl-nodem-argpm+theta
            dndt = xn-no
         end if

         xn = no + dndt

      end if

      call chkout ( 'ZZDSPC' )

      return
      end
