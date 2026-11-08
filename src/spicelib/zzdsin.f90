!$Procedure ZZDSIN ( SGP4 deep space initialization )

      subroutine zzdsin (geophs, &
                         cosim,  emsq,  argpo, s1,    s2,    s3, &
                         s4,     s5,    sinim, ss1,   ss2,   ss3, &
                         ss4,    ss5,   sz1,   sz3,   sz11,  sz13, &
                         sz21,   sz23,  sz31,  sz33,  t,     tc, &
                         gsto,   mo,    mdot,  no,    nodeo, nodedot, &
                         xpidot, z1,    z3,    z11,   z13,   z21, &
                         z23,    z31,   z33,   ecco,  eccsq, &
                         eccm,   argpm, inclm, mm,    xn,    nodem, &
                         irez,   atime, d2201, d2211, d3210, d3222, &
                         d4410,  d4422, d5220, d5232, d5421, d5433, &
                         dedt,   didt,  dmdt,  dndt,  dnodt, domdt, &
                         del1,   del2,  del3,  xfact, xlamo, xli, &
                         xni )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     This Subroutine provides Deep Space contributions to Mean
!     Motion Dot due to geopotential resonance with half day and one
!     day orbits.
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

      include 'zzsgp4.inc'

      real(wp)                 argpm
      real(wp)                 argpo
      real(wp)                 atime
      real(wp)                 cosim
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
      real(wp)                 dndt
      real(wp)                 dnodt
      real(wp)                 domdt
      real(wp)                 eccm
      real(wp)                 ecco
      real(wp)                 eccsq
      real(wp)                 emsq
      real(wp)                 geophs    ( 8 )
      real(wp)                 gsto
      real(wp)                 inclm
      real(wp)                 mdot
      real(wp)                 mm
      real(wp)                 mo
      real(wp)                 no
      real(wp)                 nodedot
      real(wp)                 nodem
      real(wp)                 nodeo
      real(wp)                 s1
      real(wp)                 s2
      real(wp)                 s3
      real(wp)                 s4
      real(wp)                 s5
      real(wp)                 sinim
      real(wp)                 ss1
      real(wp)                 ss2
      real(wp)                 ss3
      real(wp)                 ss4
      real(wp)                 ss5
      real(wp)                 sz1
      real(wp)                 sz11
      real(wp)                 sz13
      real(wp)                 sz21
      real(wp)                 sz23
      real(wp)                 sz3
      real(wp)                 sz31
      real(wp)                 sz33
      real(wp)                 t
      real(wp)                 tc
      real(wp)                 xfact
      real(wp)                 xlamo
      real(wp)                 xli
      real(wp)                 xn
      real(wp)                 xni
      real(wp)                 xpidot
      real(wp)                 z1
      real(wp)                 z11
      real(wp)                 z13
      real(wp)                 z21
      real(wp)                 z23
      real(wp)                 z3
      real(wp)                 z31
      real(wp)                 z33

      integer(ip)              irez

! $ Brief_I/O
!
!    See Detailed_input and Detailed_Output.
!
! $ Detailed_Input
!
!    COSIM       COS of mean inclination.
!
!    SINIM       SIN of mean inclination.
!
!    EMSQ        Eccentricity squared
!
!    ARGPO       Argument of Perigee
!
!    S1          S coefficients
!
!    S2              ...
!
!    S3              ...
!
!    S4              ...
!
!    S5              ...
!
!    SS1             ...
!
!    SS2             ...
!
!    SS3             ...
!
!    SS4             ...
!
!    SS5             ...
!
!    SZ1             ...
!
!    SZ3             ...
!
!    SZ11            ...
!
!    SZ13            ...
!
!    SZ21            ...
!
!    SZ23            ...
!
!    SZ31            ...
!
!    SZ33            ...
!
!    T           Time
!
!    TC
!
!    GSTO        Greenwich sidereal time in radians
!
!    MO          Mean anomaly
!
!    MDOT        Mean anomaly rate
!
!    NO          Mean motion
!
!    NODEO       Right ascension of ascending node
!
!    NODEDOT     Right ascension of ascending node rate
!
!    XPIDOT
!
!    Z1          Z coefficients
!
!    Z3               ...
!
!    Z11              ...
!
!    Z13              ...
!
!    Z21              ...
!
!    Z23              ...
!
!    Z31              ...
!
!    Z33              ...
!
!    ECCM        Mean eccentricity
!
!    ARGPM       Mean argument of perigee
!
!    INCLM       Mean inclination
!
!    MM          Mean anomaly
!
!    XN          Mean motion
!
!    NODEM       Mean right ascension of ascending node
!
! $ Detailed_Output
!
!    ECCM        Mean eccentricity
!
!    ARGPM       Mean argument of perigee
!
!    INCLM       Mean inclination
!
!    MM          Mean anomaly
!
!    XN          Mean motion
!
!    NODEM       Right ascension of ascending node
!
!    IREZ        Resonance flags: 0-none, 1-one day, 2-half day
!
!    ATIME       Internal SGD4 parameter.
!
!    D2201       D COEFFCIENTS
!
!    D2211           ...
!
!    D3210           ...
!
!    D3222           ...
!
!    D4410           ...
!
!    D4422           ...
!
!    D5220           ...
!
!    D5232           ...
!
!    D5421           ...
!
!    D5433           ...
!
!    DEDT        Internal SGD4 parameter.
!
!    DIDT        Internal SGD4 parameter.
!
!    DMDT        Internal SGD4 parameter.
!
!    DNDT        Internal SGD4 parameter.
!
!    DNODT       Internal SGD4 parameter.
!
!    DOMDT       Internal SGD4 parameter.
!
!    DEL1        Internal SGD4 parameter.
!
!    DEL2        Internal SGD4 parameter.
!
!    DEL3        Internal SGD4 parameter.
!
!    SES         Internal SGD4 parameter.
!
!    SGHL        Internal SGD4 parameter.
!
!    SGHS        Internal SGD4 parameter.
!
!    SGS         Internal SGD4 parameter.
!
!    SHL         Internal SGD4 parameter.
!
!    SHS         Internal SGD4 parameter.
!
!    SIS         Internal SGD4 parameter.
!
!    SLS         Internal SGD4 parameter.
!
!    THETA       Internal SGD4 parameter.
!
!    XFACT       Internal SGD4 parameter.
!
!    XLAMO       Internal SGD4 parameter.
!
!    XLI         Internal SGD4 parameter.
!
!    XNI         Internal SGD4 parameter.
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
!     This routine is based on the DSINIT code by David Vallado
!     corresponding to "Revisiting Spacetrack Report #3" [4].
!     The intent is to maintain the original Vallado algorithm,
!     changing code only to meet NAIF format standards and to
!     integrate with SPICELIB.
!
!        Removed getgravconst call, replaced with GEOPHS array.
!
!        Capitalize all variables.
!
!        ENDIF replaced with END IF.
!
!        whichconst    eliminated, function provided by GEOPHS
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
!-    SPICELIB Version 1.0.0, 03-NOV-2014 (EDW)
!
!        Based on routine DSINIT, 28-JUN-2005, Vallado 2006 [4].
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
      real(wp)                 ainv2
      real(wp)                 aonv
      real(wp)                 cosisq
      real(wp)                 emo
      real(wp)                 emsqo
      real(wp)                 eoc
      real(wp)                 f220
      real(wp)                 f221
      real(wp)                 f311
      real(wp)                 f321
      real(wp)                 f322
      real(wp)                 f330
      real(wp)                 f441
      real(wp)                 f442
      real(wp)                 f522
      real(wp)                 f523
      real(wp)                 f542
      real(wp)                 f543
      real(wp)                 g200
      real(wp)                 g201
      real(wp)                 g211
      real(wp)                 g300
      real(wp)                 g310
      real(wp)                 g322
      real(wp)                 g410
      real(wp)                 g422
      real(wp)                 g520
      real(wp)                 g521
      real(wp)                 g532
      real(wp)                 g533
      real(wp)                 q22
      real(wp)                 q31
      real(wp)                 q33
      real(wp)                 root22
      real(wp)                 root32
      real(wp)                 root44
      real(wp)                 root52
      real(wp)                 root54
      real(wp)                 rptim
      real(wp)                 ses
      real(wp)                 sghl
      real(wp)                 sghs
      real(wp)                 sgs
      real(wp)                 shl
      real(wp)                 shs
      real(wp)                 sini2
      real(wp)                 sis
      real(wp)                 sls
      real(wp)                 temp
      real(wp)                 temp1
      real(wp)                 theta
      real(wp)                 x2o3
      real(wp)                 xke
      real(wp)                 xno2
      real(wp)                 znl
      real(wp)                 zns



!
!     SPICELIB routines.
!
      real(wp)                 pi
      real(wp)                 twopi
      logical                  return


!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'ZZDSIN' )

!
!     Constants
!
      q22    = 1.7891679e-6_wp
      q31    = 2.1460748e-6_wp
      q33    = 2.2123015e-7_wp
      root22 = 1.7891679e-6_wp
      root44 = 7.3636953e-9_wp
      root54 = 2.1765803e-9_wp

!
!     This equates to 7.29211514668855e-5 rad/sec
!
      rptim  = 4.37526908801129966e-3_wp

      root32 = 3.7393792e-7_wp
      root52 = 1.1428639e-7_wp
      x2o3   = 2.0_wp / 3.0_wp
      znl    = 1.5835218e-4_wp
      zns    = 1.19459e-5_wp

!
!     This code block replaces the call:
!
!     sgp4fix identify constants and allow alternate values.
!
!     CALL getgravconst( whichconst, tumin,
!     .                  mu, radiusearthkm, xke,
!     .                  j2, j3, j4, j3oj2 )
!
      xke   = geophs(k_ke)

!
!     DEEP SPACE INITIALIZATION
!
      irez = 0
      if ( (xn<0.0052359877_wp)  .and. &
           (xn>0.0034906585_wp) ) then
         irez = 1
      end if

      if ( (xn   >= 8.26e-3_wp) .and. &
           (xn   <= 9.24e-3_wp) .and. &
           (eccm >= 0.5_wp)  )then
         irez = 2
      end if

!
!     DO SOLAR TERMS
!
      ses  =  ss1*zns*ss5
      sis  =  ss2*zns*(sz11 + sz13)
      sls  = -zns*ss3*(sz1 + sz3 - 14.0_wp - 6.0_wp*emsq)
      sghs =  ss4*zns*(sz31 + sz33 - 6.0_wp)
      shs  = -zns*ss2*(sz21 + sz23)

!
!       sgp4fix for 180 deg incl
!
        if ( (inclm < 5.2359877e-2_wp) .or. &
             (inclm > pi()-5.2359877e-2_wp) ) then
            shs = 0.0_wp
        end if

        if (sinim/=0.0_wp) then
            shs = shs/sinim
        end if

        sgs  = sghs - cosim*shs

!
!       DO LUNAR TERMS
!
        dedt = ses + s1*znl*s5
        didt = sis + s2*znl*(z11 + z13)
        dmdt = sls - znl*s3*(z1 + z3 - 14.0_wp - 6.0_wp*emsq)
        sghl = s4*znl*(z31 + z33 - 6.0_wp)
        shl  = -znl*s2*(z21 + z23)

!
!       sgp4fix for 180 deg incl
!

        if ( (inclm < 5.2359877e-2_wp ) .or. &
             (inclm > pi()-5.2359877e-2_wp)) then
            shl = 0.0_wp
        end if

        domdt= sgs+sghl
        dnodt= shs

        if (sinim /= 0.0_wp) then
            domdt = domdt-cosim/sinim*shl
            dnodt = dnodt+shl/sinim
        end if

!
!       CALCULATE DEEP SPACE RESONANCE EFFECTS
!
        dndt  = 0.0_wp
        theta = dmod(gsto + tc*rptim, twopi() )
        eccm  = eccm + dedt*t
        emsq  = eccm**2
        inclm = inclm + didt*t
        argpm = argpm + domdt*t
        nodem = nodem + dnodt*t
        mm    = mm + dmdt*t

!
!   sgp4fix for negative inclinations
!   the following if statement should be commented out
!
!           IF(Inclm .lt. 0.0D0) THEN
!             Inclm  = -Inclm
!             Argpm  = Argpm-PI
!             nodem = nodem+PI
!           END IF
!

!
!       Initialize the resonance terms
!
         if (irez /= 0) then

            aonv = (xn/xke)**x2o3

!
!           GEOPOTENTIAL RESONANCE FOR 12 HOUR ORBITS
!
            if (irez == 2) then

               cosisq = cosim*cosim
               emo    = eccm
               emsqo  = emsq
               eccm   = ecco
               emsq   = eccsq
               eoc    = eccm*emsq
               g201   = -0.306_wp-(eccm-0.64_wp)*0.440_wp

               if (eccm<=0.65_wp) then

                  g211 =   3.616_wp -  13.2470_wp*eccm + &
                              16.2900_wp*emsq
                  g310 = -19.302_wp + 117.3900_wp*eccm - &
                              228.4190_wp*emsq + 156.591_wp*eoc
                  g322 = -18.9068_wp+ 109.7927_wp*eccm - &
                              214.6334_wp*emsq + 146.5816_wp*eoc
                  g410 = -41.122_wp + 242.6940_wp*eccm - &
                              471.0940_wp*emsq + 313.953_wp*eoc
                  g422 =-146.407_wp + 841.8800_wp*eccm - &
                              1629.014_wp*emsq + 1083.435_wp*eoc
                  g520 =-532.114_wp + 3017.977_wp*eccm - &
                              5740.032_wp*emsq + 3708.276_wp*eoc

               else

                  g211 =  -72.099_wp +  331.819_wp*eccm - &
                              508.738_wp*emsq + 266.724_wp*eoc
                  g310 = -346.844_wp + 1582.851_wp*eccm - &
                              2415.925_wp*emsq + 1246.113_wp*eoc
                  g322 = -342.585_wp + 1554.908_wp*eccm - &
                              2366.899_wp*emsq + 1215.972_wp*eoc
                  g410 =-1052.797_wp + 4758.686_wp*eccm - &
                              7193.992_wp*emsq + 3651.957_wp*eoc
                  g422 =-3581.690_wp + 16178.11_wp*eccm - &
                              24462.77_wp*emsq + 12422.52_wp*eoc

                  if (eccm>0.715_wp) then
                     g520 =-5149.66_wp + 29936.92_wp*eccm - &
                              54087.36_wp*emsq + 31324.56_wp*eoc
                  else
                     g520 = 1464.74_wp -  4664.75_wp*eccm + &
                              3763.64_wp*emsq
                  end if

               end if

            if (eccm<0.7_wp) then

                g533 = -919.22770_wp + 4988.6100_wp*eccm-9064.7700_wp*emsq &
                     + 5542.21_wp*eoc
                g521 = -822.71072_wp + 4568.6173_wp*eccm-8491.4146_wp*emsq &
                     + 5337.524_wp*eoc
                g532 = -853.66600_wp + 4690.2500_wp*eccm-8624.7700_wp*emsq &
                     + 5341.4_wp*eoc

            else

                g533 =-37995.780_wp + 161616.52_wp*eccm-229838.20_wp*emsq+ &
                    109377.94_wp*eoc
                g521 =-51752.104_wp + 218913.95_wp*eccm-309468.16_wp*emsq+ &
                    146349.42_wp*eoc
                g532 =-40023.880_wp + 170470.89_wp*eccm-242699.48_wp*emsq+ &
                    115605.82_wp*eoc

            end if

            sini2 =  sinim*sinim
            f220  =  0.75_wp* (1.0_wp+2.0_wp*cosim+cosisq)
            f221  =  1.5_wp*sini2
            f321  =  1.875_wp*sinim * (1.0_wp-2.0_wp*cosim-3.0_wp*cosisq)
            f322  = -1.875_wp*sinim * (1.0_wp+2.0_wp*cosim-3.0_wp*cosisq)
            f441  = 35.0_wp*sini2*f220
            f442  = 39.3750_wp*sini2*sini2
            f522  =  9.84375_wp*sinim * (sini2* (1.0_wp-2.0_wp*cosim- &
                     5.0_wp*cosisq)+0.33333333_wp * (-2.0_wp+4.0_wp*cosim+ &
                     6.0_wp*cosisq) )
            f523  =  sinim * (4.92187512_wp*sini2 * (-2.0_wp-4.0_wp*cosim+ &
                     10.0_wp*cosisq) + 6.56250012_wp* &
                     (1.0_wp+2.0_wp*cosim-3.0_wp*cosisq))
            f542  =  29.53125_wp*sinim * (2.0_wp-8.0_wp*cosim+cosisq* &
                     (-12.0_wp+8.0_wp*cosim+10.0_wp*cosisq) )
            f543  = 29.53125_wp*sinim * (-2.0_wp-8.0_wp*cosim+cosisq* &
                     (12.0_wp+8.0_wp*cosim-10.0_wp*cosisq) )

            xno2   =  xn * xn
            ainv2  =  aonv * aonv
            temp1  =  3.0_wp*xno2*ainv2
            temp   =  temp1*root22
            d2201  =  temp*f220*g201
            d2211  =  temp*f221*g211
            temp1  =  temp1*aonv
            temp   =  temp1*root32
            d3210  =  temp*f321*g310
            d3222  =  temp*f322*g322
            temp1  =  temp1*aonv
            temp   =  2.0_wp*temp1*root44
            d4410  =  temp*f441*g410
            d4422  =  temp*f442*g422
            temp1  =  temp1*aonv
            temp   =  temp1*root52
            d5220  =  temp*f522*g520
            d5232  =  temp*f523*g532
            temp   =  2.0_wp*temp1*root54
            d5421  =  temp*f542*g521
            d5433  =  temp*f543*g533
            xlamo  =  dmod(mo+nodeo+nodeo-theta-theta, twopi())
            xfact  = mdot + dmdt + 2.0_wp * (nodedot+dnodt-rptim) - no

            eccm = emo
            emsq = emsqo

         end if

         if (irez == 1) then

!
!           SYNCHRONOUS RESONANCE TERMS
!
            g200  = 1.0_wp + emsq * (-2.5_wp+0.8125_wp*emsq)
            g310  = 1.0_wp + 2.0_wp*emsq
            g300  = 1.0_wp + emsq * (-6.0_wp+6.60937_wp*emsq)
            f220  = 0.75_wp * (1.0_wp+cosim) * (1.0_wp+cosim)
            f311  = 0.9375_wp*sinim*sinim* &
                     (1.0_wp+3.0_wp*cosim) - 0.75_wp*(1.0_wp+cosim)
            f330  = 1.0_wp+cosim
            f330  = 1.875_wp*f330*f330*f330
            del1  = 3.0_wp*xn*xn*aonv*aonv
            del2  = 2.0_wp*del1*f220*g200*q22
            del3  = 3.0_wp*del1*f330*g300*q33*aonv
            del1  = del1*f311*g310*q31*aonv
            xlamo = dmod(mo+nodeo+argpo-theta, twopi())
            xfact = mdot + xpidot - rptim + dmdt + domdt + dnodt - no

         end if

!
!        FOR SGP4, INITIALIZE THE INTEGRATOR
!
         xli   = xlamo
         xni   = no
         atime = 0.0_wp
         xn    = no + dndt

      end if

      call chkout ( 'ZZDSIN' )

      return
      end
