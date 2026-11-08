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

      double precision         argpm
      double precision         argpo
      double precision         atime
      double precision         cosim
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
      double precision         dndt
      double precision         dnodt
      double precision         domdt
      double precision         eccm
      double precision         ecco
      double precision         eccsq
      double precision         emsq
      double precision         geophs    ( 8 )
      double precision         gsto
      double precision         inclm
      double precision         mdot
      double precision         mm
      double precision         mo
      double precision         no
      double precision         nodedot
      double precision         nodem
      double precision         nodeo
      double precision         s1
      double precision         s2
      double precision         s3
      double precision         s4
      double precision         s5
      double precision         sinim
      double precision         ss1
      double precision         ss2
      double precision         ss3
      double precision         ss4
      double precision         ss5
      double precision         sz1
      double precision         sz11
      double precision         sz13
      double precision         sz21
      double precision         sz23
      double precision         sz3
      double precision         sz31
      double precision         sz33
      double precision         t
      double precision         tc
      double precision         xfact
      double precision         xlamo
      double precision         xli
      double precision         xn
      double precision         xni
      double precision         xpidot
      double precision         z1
      double precision         z11
      double precision         z13
      double precision         z21
      double precision         z23
      double precision         z3
      double precision         z31
      double precision         z33

      integer                  irez

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
      double precision         ainv2
      double precision         aonv
      double precision         cosisq
      double precision         emo
      double precision         emsqo
      double precision         eoc
      double precision         f220
      double precision         f221
      double precision         f311
      double precision         f321
      double precision         f322
      double precision         f330
      double precision         f441
      double precision         f442
      double precision         f522
      double precision         f523
      double precision         f542
      double precision         f543
      double precision         g200
      double precision         g201
      double precision         g211
      double precision         g300
      double precision         g310
      double precision         g322
      double precision         g410
      double precision         g422
      double precision         g520
      double precision         g521
      double precision         g532
      double precision         g533
      double precision         q22
      double precision         q31
      double precision         q33
      double precision         root22
      double precision         root32
      double precision         root44
      double precision         root52
      double precision         root54
      double precision         rptim
      double precision         ses
      double precision         sghl
      double precision         sghs
      double precision         sgs
      double precision         shl
      double precision         shs
      double precision         sini2
      double precision         sis
      double precision         sls
      double precision         temp
      double precision         temp1
      double precision         theta
      double precision         x2o3
      double precision         xke
      double precision         xno2
      double precision         znl
      double precision         zns



!
!     SPICELIB routines.
!
      double precision         pi
      double precision         twopi
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
      q22    = 1.7891679d-6
      q31    = 2.1460748d-6
      q33    = 2.2123015d-7
      root22 = 1.7891679d-6
      root44 = 7.3636953d-9
      root54 = 2.1765803d-9

!
!     This equates to 7.29211514668855e-5 rad/sec
!
      rptim  = 4.37526908801129966d-3

      root32 = 3.7393792d-7
      root52 = 1.1428639d-7
      x2o3   = 2.0d0 / 3.0d0
      znl    = 1.5835218d-4
      zns    = 1.19459d-5

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
      if ( (xn.lt.0.0052359877d0)  .and. &
           (xn.gt.0.0034906585d0) ) then
         irez = 1
      end if

      if ( (xn   .ge. 8.26d-3) .and. &
           (xn   .le. 9.24d-3) .and. &
           (eccm .ge. 0.5d0)  )then
         irez = 2
      end if

!
!     DO SOLAR TERMS
!
      ses  =  ss1*zns*ss5
      sis  =  ss2*zns*(sz11 + sz13)
      sls  = -zns*ss3*(sz1 + sz3 - 14.0d0 - 6.0d0*emsq)
      sghs =  ss4*zns*(sz31 + sz33 - 6.0d0)
      shs  = -zns*ss2*(sz21 + sz23)

!
!       sgp4fix for 180 deg incl
!
        if ( (inclm .lt. 5.2359877d-2) .or. &
             (inclm .gt. pi()-5.2359877d-2) ) then
            shs = 0.0d0
        end if

        if (sinim.ne.0.0d0) then
            shs = shs/sinim
        end if

        sgs  = sghs - cosim*shs

!
!       DO LUNAR TERMS
!
        dedt = ses + s1*znl*s5
        didt = sis + s2*znl*(z11 + z13)
        dmdt = sls - znl*s3*(z1 + z3 - 14.0d0 - 6.0d0*emsq)
        sghl = s4*znl*(z31 + z33 - 6.0d0)
        shl  = -znl*s2*(z21 + z23)

!
!       sgp4fix for 180 deg incl
!

        if ( (inclm .lt. 5.2359877d-2 ) .or. &
             (inclm .gt. pi()-5.2359877d-2)) then
            shl = 0.0d0
        end if

        domdt= sgs+sghl
        dnodt= shs

        if (sinim .ne. 0.0d0) then
            domdt = domdt-cosim/sinim*shl
            dnodt = dnodt+shl/sinim
        end if

!
!       CALCULATE DEEP SPACE RESONANCE EFFECTS
!
        dndt  = 0.0d0
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
         if (irez .ne. 0) then

            aonv = (xn/xke)**x2o3

!
!           GEOPOTENTIAL RESONANCE FOR 12 HOUR ORBITS
!
            if (irez .eq. 2) then

               cosisq = cosim*cosim
               emo    = eccm
               emsqo  = emsq
               eccm   = ecco
               emsq   = eccsq
               eoc    = eccm*emsq
               g201   = -0.306d0-(eccm-0.64d0)*0.440d0

               if (eccm.le.0.65d0) then

                  g211 =   3.616d0 -  13.2470d0*eccm + &
                              16.2900d0*emsq
                  g310 = -19.302d0 + 117.3900d0*eccm - &
                              228.4190d0*emsq + 156.591d0*eoc
                  g322 = -18.9068d0+ 109.7927d0*eccm - &
                              214.6334d0*emsq + 146.5816d0*eoc
                  g410 = -41.122d0 + 242.6940d0*eccm - &
                              471.0940d0*emsq + 313.953d0*eoc
                  g422 =-146.407d0 + 841.8800d0*eccm - &
                              1629.014d0*emsq + 1083.435d0*eoc
                  g520 =-532.114d0 + 3017.977d0*eccm - &
                              5740.032d0*emsq + 3708.276d0*eoc

               else

                  g211 =  -72.099d0 +  331.819d0*eccm - &
                              508.738d0*emsq + 266.724d0*eoc
                  g310 = -346.844d0 + 1582.851d0*eccm - &
                              2415.925d0*emsq + 1246.113d0*eoc
                  g322 = -342.585d0 + 1554.908d0*eccm - &
                              2366.899d0*emsq + 1215.972d0*eoc
                  g410 =-1052.797d0 + 4758.686d0*eccm - &
                              7193.992d0*emsq + 3651.957d0*eoc
                  g422 =-3581.690d0 + 16178.11d0*eccm - &
                              24462.77d0*emsq + 12422.52d0*eoc

                  if (eccm.gt.0.715d0) then
                     g520 =-5149.66d0 + 29936.92d0*eccm - &
                              54087.36d0*emsq + 31324.56d0*eoc
                  else
                     g520 = 1464.74d0 -  4664.75d0*eccm + &
                              3763.64d0*emsq
                  end if

               end if

            if (eccm.lt.0.7d0) then

                g533 = -919.22770d0 + 4988.6100d0*eccm-9064.7700d0*emsq &
                     + 5542.21d0*eoc
                g521 = -822.71072d0 + 4568.6173d0*eccm-8491.4146d0*emsq &
                     + 5337.524d0*eoc
                g532 = -853.66600d0 + 4690.2500d0*eccm-8624.7700d0*emsq &
                     + 5341.4d0*eoc

            else

                g533 =-37995.780d0 + 161616.52d0*eccm-229838.20d0*emsq+ &
                    109377.94d0*eoc
                g521 =-51752.104d0 + 218913.95d0*eccm-309468.16d0*emsq+ &
                    146349.42d0*eoc
                g532 =-40023.880d0 + 170470.89d0*eccm-242699.48d0*emsq+ &
                    115605.82d0*eoc

            end if

            sini2 =  sinim*sinim
            f220  =  0.75d0* (1.0d0+2.0d0*cosim+cosisq)
            f221  =  1.5d0*sini2
            f321  =  1.875d0*sinim * (1.0d0-2.0d0*cosim-3.0d0*cosisq)
            f322  = -1.875d0*sinim * (1.0d0+2.0d0*cosim-3.0d0*cosisq)
            f441  = 35.0d0*sini2*f220
            f442  = 39.3750d0*sini2*sini2
            f522  =  9.84375d0*sinim * (sini2* (1.0d0-2.0d0*cosim- &
                     5.0d0*cosisq)+0.33333333d0 * (-2.0d0+4.0d0*cosim+ &
                     6.0d0*cosisq) )
            f523  =  sinim * (4.92187512d0*sini2 * (-2.0d0-4.0d0*cosim+ &
                     10.0d0*cosisq) + 6.56250012d0* &
                     (1.0d0+2.0d0*cosim-3.0d0*cosisq))
            f542  =  29.53125d0*sinim * (2.0d0-8.0d0*cosim+cosisq* &
                     (-12.0d0+8.0d0*cosim+10.0d0*cosisq) )
            f543  = 29.53125d0*sinim * (-2.0d0-8.0d0*cosim+cosisq* &
                     (12.0d0+8.0d0*cosim-10.0d0*cosisq) )

            xno2   =  xn * xn
            ainv2  =  aonv * aonv
            temp1  =  3.0d0*xno2*ainv2
            temp   =  temp1*root22
            d2201  =  temp*f220*g201
            d2211  =  temp*f221*g211
            temp1  =  temp1*aonv
            temp   =  temp1*root32
            d3210  =  temp*f321*g310
            d3222  =  temp*f322*g322
            temp1  =  temp1*aonv
            temp   =  2.0d0*temp1*root44
            d4410  =  temp*f441*g410
            d4422  =  temp*f442*g422
            temp1  =  temp1*aonv
            temp   =  temp1*root52
            d5220  =  temp*f522*g520
            d5232  =  temp*f523*g532
            temp   =  2.0d0*temp1*root54
            d5421  =  temp*f542*g521
            d5433  =  temp*f543*g533
            xlamo  =  dmod(mo+nodeo+nodeo-theta-theta, twopi())
            xfact  = mdot + dmdt + 2.0d0 * (nodedot+dnodt-rptim) - no

            eccm = emo
            emsq = emsqo

         end if

         if (irez .eq. 1) then

!
!           SYNCHRONOUS RESONANCE TERMS
!
            g200  = 1.0d0 + emsq * (-2.5d0+0.8125d0*emsq)
            g310  = 1.0d0 + 2.0d0*emsq
            g300  = 1.0d0 + emsq * (-6.0d0+6.60937d0*emsq)
            f220  = 0.75d0 * (1.0d0+cosim) * (1.0d0+cosim)
            f311  = 0.9375d0*sinim*sinim* &
                     (1.0d0+3.0d0*cosim) - 0.75d0*(1.0d0+cosim)
            f330  = 1.0d0+cosim
            f330  = 1.875d0*f330*f330*f330
            del1  = 3.0d0*xn*xn*aonv*aonv
            del2  = 2.0d0*del1*f220*g200*q22
            del3  = 3.0d0*del1*f330*g300*q33*aonv
            del1  = del1*f311*g310*q31*aonv
            xlamo = dmod(mo+nodeo+argpo-theta, twopi())
            xfact = mdot + xpidot - rptim + dmdt + domdt + dnodt - no

         end if

!
!        FOR SGP4, INITIALIZE THE INTEGRATOR
!
         xli   = xlamo
         xni   = no
         atime = 0.0d0
         xn    = no + dndt

      end if

      call chkout ( 'ZZDSIN' )

      return
      end
