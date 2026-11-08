!$Procedure ZZDSCM ( SGP4 deep space common calculations )

      subroutine zzdscm ( epoch,  eccp,  argpp,   tc,     inclp, &
                          nodep,  np,    snodm,   cnodm, &
                          sinim,  cosim, sinomm,  cosomm, &
                          day,    e3,    ee2,     eccm,   emsq, gam, &
                          peo,    pgho,  pho,     pinco,  plo, &
                          rtemsq, se2,   se3,     sgh2,   sgh3, sgh4, &
                          sh2,    sh3,   si2,     si3,    sl2,  sl3, &
                          sl4,    s1,    s2,      s3,     s4,   s5, &
                          s6,     s7,    ss1,     ss2,    ss3,  ss4, &
                          ss5,    ss6,   ss7,     sz1,    sz2,  sz3, &
                          sz11,   sz12,  sz13,    sz21,   sz22, sz23, &
                          sz31,   sz32,  sz33,    xgh2,   xgh3, xgh4, &
                          xh2,    xh3,   xi2,     xi3,    xl2,  xl3, &
                          xl4,    xn,    z1,      z2,     z3,   z11, &
                          z12,    z13,   z21,     z22,    z23,  z31, &
                          z32,    z33,   zmol,    zmos )

! $ Abstract
!
!     This subroutine provides deep space common items used by both the
!     secular and periodics subroutines.
!
!     This routine previously had the name DPPER, but the functions
!     inside weren't well organized.
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

      double precision         epoch
      double precision         eccp
      double precision         argpp
      double precision         tc
      double precision         inclp
      double precision         nodep
      double precision         np
      double precision         snodm
      double precision         cnodm
      double precision         sinim
      double precision         cosim
      double precision         sinomm
      double precision         cosomm
      double precision         day
      double precision         e3
      double precision         ee2
      double precision         eccm
      double precision         emsq
      double precision         gam
      double precision         rtemsq
      double precision         se2
      double precision         peo
      double precision         pgho
      double precision         pho
      double precision         pinco
      double precision         plo
      double precision         se3
      double precision         sgh2
      double precision         sgh3
      double precision         sgh4
      double precision         sh2
      double precision         sh3
      double precision         si2
      double precision         si3
      double precision         sl2
      double precision         sl3
      double precision         sl4
      double precision         s1
      double precision         s2
      double precision         s3
      double precision         s4
      double precision         s5
      double precision         s6
      double precision         s7
      double precision         ss1
      double precision         ss2
      double precision         ss3
      double precision         ss4
      double precision         ss5
      double precision         ss6
      double precision         ss7
      double precision         sz1
      double precision         sz2
      double precision         sz3
      double precision         sz11
      double precision         sz12
      double precision         sz13
      double precision         sz21
      double precision         sz22
      double precision         sz23
      double precision         sz31
      double precision         sz32
      double precision         sz33
      double precision         xgh2
      double precision         xgh3
      double precision         xgh4
      double precision         xh2
      double precision         xh3
      double precision         xi2
      double precision         xi3
      double precision         xl2
      double precision         xl3
      double precision         xl4
      double precision         xn
      double precision         z1
      double precision         z2
      double precision         z3
      double precision         z11
      double precision         z12
      double precision         z13
      double precision         z21
      double precision         z22
      double precision         z23
      double precision         z31
      double precision         z32
      double precision         z33
      double precision         zmol
      double precision         zmos

! $ Brief_I/O
!
!    See Detailed_input and Detailed_Output.
!
! $ Detailed_Input
!
!    EPOCH       Epoch of TLE set as Julian day value.
!
!    EP          Eccentricity
!
!    ARGPP       Argument of perigee
!
!    TC          Minutes past EPOCH, nominally zero.
!
!    INCLP       Inclination
!
!    NODEP       Right ascension of ascending node
!
!    NP          Mean motion
!
! $ Detailed_Output
!
!    SINIM       SIN of mean inclination.
!
!    COSIM       COS of mean inclination.
!
!    SINOMM      Internal SGD4 parameter.
!
!    COSOMM      Internal SGD4 parameter.
!
!    SNODM       Internal SGD4 parameter.
!
!    CNODM       Internal SGD4 parameter.
!
!    DAY         Internal SGD4 parameter.
!
!    E3          Internal SGD4 parameter.
!
!    EE2         Internal SGD4 parameter.
!
!    EM          Eccentricity.
!
!    EMSQ        Eccentricity squared.
!
!    GAM         Internal SGD4 parameter.
!
!    PEO         Internal SGD4 parameter.
!
!    PGHO        Internal SGD4 parameter.
!
!    PHO         Internal SGD4 parameter.
!
!    PINCO       Internal SGD4 parameter.
!
!    PLO         Internal SGD4 parameter.
!
!    RTEMSQ      Internal SGD4 parameter.
!
!    SE2         Internal SGD4 parameter.
!
!    SE3         Internal SGD4 parameter.
!
!    SGH2        Internal SGD4 parameter.
!
!    SGH3        Internal SGD4 parameter.
!
!    SGH4        Internal SGD4 parameter.
!
!    SH2         Internal SGD4 parameter.
!
!    SH3         Internal SGD4 parameter.
!
!    SI2         Internal SGD4 parameter.
!
!    SI3         Internal SGD4 parameter.
!
!    SL2         Internal SGD4 parameter.
!
!    SL3         Internal SGD4 parameter.
!
!    SL4         Internal SGD4 parameter.
!
!    S1          S coefficients
!
!    S2             ...
!
!    S3             ...
!
!    S4             ...
!
!    S5             ...
!
!    S6             ...
!
!    S7             ...
!
!    SS1            ...
!
!    SS2            ...
!
!    SS3            ...
!
!    SS4            ...
!
!    SS5            ...
!
!    SS6            ...
!
!    SS7            ...
!
!    SZ1            ...
!
!    SZ2            ...
!
!    SZ3            ...
!
!    SZ11           ...
!
!    SZ12           ...
!
!    SZ13           ...
!
!    SZ21           ...
!
!    SZ22           ...
!
!    SZ23           ...
!
!    SZ31           ...
!
!    SZ32           ...
!
!    SZ33           ...
!
!    XGH2        Internal SGD4 parameter.
!
!    XGH3        Internal SGD4 parameter.
!
!    XGH4        Internal SGD4 parameter.
!
!    XH2         Internal SGD4 parameter.
!
!    XH3         Internal SGD4 parameter.
!
!    XI2         Internal SGD4 parameter.
!
!    XI3         Internal SGD4 parameter.
!
!    XL2         Internal SGD4 parameter.
!
!    XL3         Internal SGD4 parameter.
!
!    XL4         Internal SGD4 parameter.
!
!    NM          Mean motion
!
!    Z1          Z coefficients
!
!    Z2             ...
!
!    Z3             ...
!
!    Z11            ...
!
!    Z12            ...
!
!    Z13            ...
!
!    Z21            ...
!
!    Z22            ...
!
!    Z23            ...
!
!    Z31            ...
!
!    Z32            ...
!
!    Z33            ...
!
!    ZMOL        Internal SGD4 parameter.
!
!    ZMOS        Internal SGD4 parameter.
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
!     This routine is based on the DSCOM code by David Vallado
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
!-    SPICELIB Version 1.0.0, 11-NOV-2014 (EDW)
!
!        Based on routine DSCOM, 28-JUN-2005, Vallado 2006 [4].
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
      double precision         zes
      double precision         a1
      double precision         a10
      double precision         a2
      double precision         a3
      double precision         a4
      double precision         a5
      double precision         a6
      double precision         a7
      double precision         a8
      double precision         a9
      double precision         betasq
      double precision         c1l
      double precision         c1ss
      double precision         cc
      double precision         ctem
      double precision         stem
      double precision         x1
      double precision         x2
      double precision         x3
      double precision         x4
      double precision         x5
      double precision         x6
      double precision         x7
      double precision         x8
      double precision         xnodce
      double precision         xnoi
      double precision         zcosg
      double precision         zcosgl
      double precision         zcosgs
      double precision         zcosh
      double precision         zcoshl
      double precision         zcosi
      double precision         zcosil
      double precision         zcosis
      double precision         zel
      double precision         zsing
      double precision         zsingl
      double precision         zsings
      double precision         zsinh
      double precision         zsinhl
      double precision         zsini
      double precision         zsinil
      double precision         zsinis
      double precision         zx
      double precision         zy

      integer                  lsflg

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

      call chkin ( 'ZZDSCM' )

!
!     Constants
!
      zes    =  0.01675d0
      zel    =  0.05490d0
      c1ss   =  2.9864797d-6
      c1l    =  4.7968065d-7
      zsinis =  0.39785416d0
      zcosis =  0.91744867d0
      zcosgs =  0.1945905d0
      zsings = -0.98088458d0

!
!     DEEP SPACE PERIODICS INITIALIZATION
!
      xn     = np
      eccm   = eccp
      snodm  = dsin(nodep)
      cnodm  = dcos(nodep)
      sinomm = dsin(argpp)
      cosomm = dcos(argpp)
      sinim  = dsin(inclp)
      cosim  = dcos(inclp)
      emsq   = eccm*eccm
      betasq = 1.0d0-emsq
      rtemsq = dsqrt(betasq)

!
!     INITIALIZE LUNAR SOLAR TERMS
!
!     Note, EPOCH + 18261.5D0 corresponds to JD days since
!     1899-12-31 12:00:00.
!
      peo    = 0.0d0
      pinco  = 0.0d0
      plo    = 0.0d0
      pgho   = 0.0d0
      pho    = 0.0d0
      day    = epoch + 18261.5d0 + tc/1440.0d0
      xnodce = dmod(4.5236020d0 - 9.2422029d-4*day,twopi())
      stem   = dsin(xnodce)
      ctem   = dcos(xnodce)
      zcosil = 0.91375164d0 - 0.03568096d0*ctem
      zsinil = dsqrt(1.0d0 - zcosil*zcosil)
      zsinhl = 0.089683511d0*stem / zsinil
      zcoshl = dsqrt(1.0d0 - zsinhl*zsinhl)
      gam    = 5.8351514d0 + 0.0019443680d0*day
      zx     = 0.39785416d0*stem/zsinil
      zy     = zcoshl*ctem + 0.91744867d0*zsinhl*stem
      zx     = datan2(zx,zy)
      zx     = gam + zx - xnodce
      zcosgl = dcos(zx)
      zsingl = dsin(zx)

!
!     DO SOLAR TERMS
!
      zcosg = zcosgs
      zsing = zsings
      zcosi = zcosis
      zsini = zsinis
      zcosh = cnodm
      zsinh = snodm
      cc    = c1ss
      xnoi  = 1.0d0 / xn

!
!     Loop over the lunar and solar term flags.
!
      do lsflg = 1,2

         a1 =   zcosg*zcosh + zsing*zcosi*zsinh
         a3 =  -zsing*zcosh + zcosg*zcosi*zsinh
         a7 =  -zcosg*zsinh + zsing*zcosi*zcosh
         a8 =   zsing*zsini
         a9 =   zsing*zsinh + zcosg*zcosi*zcosh
         a10=   zcosg*zsini
         a2 =   cosim*a7 + sinim*a8
         a4 =   cosim*a9 + sinim*a10
         a5 =  -sinim*a7 + cosim*a8
         a6 =  -sinim*a9 + cosim*a10

         x1 =  a1*cosomm + a2*sinomm
         x2 =  a3*cosomm + a4*sinomm
         x3 = -a1*sinomm + a2*cosomm
         x4 = -a3*sinomm + a4*cosomm
         x5 =  a5*sinomm
         x6 =  a6*sinomm
         x7 =  a5*cosomm
         x8 =  a6*cosomm

         z31= 12.0d0*x1*x1 - 3.0d0*x3*x3
         z32= 24.0d0*x1*x2 - 6.0d0*x3*x4
         z33= 12.0d0*x2*x2 - 3.0d0*x4*x4
         z1 =  3.0d0* (a1*a1 + a2*a2) + z31*emsq
         z2 =  6.0d0* (a1*a3 + a2*a4) + z32*emsq
         z3 =  3.0d0* (a3*a3 + a4*a4) + z33*emsq
         z11= -6.0d0*a1*a5 + emsq* (-24.0d0*x1*x7-6.0d0*x3*x5)
         z12= -6.0d0* (a1*a6 + a3*a5) + emsq* &
                 ( -24.0d0*(x2*x7+x1*x8) - 6.0d0*(x3*x6+x4*x5) )
         z13= -6.0d0*a3*a6 + emsq*(-24.0d0*x2*x8 - 6.0d0*x4*x6)
         z21=  6.0d0*a2*a5 + emsq*(24.0d0*x1*x5-6.0d0*x3*x7)
         z22=  6.0d0* (a4*a5 + a2*a6) + emsq* &
                 (  24.0d0*(x2*x5+x1*x6) - 6.0d0*(x4*x7+x3*x8) )
         z23=  6.0d0*a4*a6 + emsq*(24.0d0*x2*x6 - 6.0d0*x4*x8)
         z1 = z1 + z1 + betasq*z31
         z2 = z2 + z2 + betasq*z32
         z3 = z3 + z3 + betasq*z33
         s3 = cc*xnoi
         s2 = -0.5d0*s3 / rtemsq
         s4 = s3*rtemsq
         s1 = -15.0d0*eccm*s4
         s5 = x1*x3 + x2*x4
         s6 = x2*x3 + x1*x4
         s7 = x2*x4 - x1*x3

!
!        DO LUNAR TERMS
!
         if (lsflg.eq.1) then
            ss1   = s1
            ss2   = s2
            ss3   = s3
            ss4   = s4
            ss5   = s5
            ss6   = s6
            ss7   = s7
            sz1   = z1
            sz2   = z2
            sz3   = z3
            sz11  = z11
            sz12  = z12
            sz13  = z13
            sz21  = z21
            sz22  = z22
            sz23  = z23
            sz31  = z31
            sz32  = z32
            sz33  = z33
            zcosg = zcosgl
            zsing = zsingl
            zcosi = zcosil
            zsini = zsinil
            zcosh = zcoshl*cnodm+zsinhl*snodm
            zsinh = snodm*zcoshl-cnodm*zsinhl
            cc    = c1l
         end if

      end do

      zmol  = dmod( 4.7199672d0 + 0.22997150d0*day-gam, twopi() )
      zmos  = dmod( 6.2565837d0 + 0.017201977d0*day,    twopi() )

!
!     DO SOLAR TERMS
!
      se2 =   2.0d0*ss1*ss6
      se3 =   2.0d0*ss1*ss7
      si2 =   2.0d0*ss2*sz12
      si3 =   2.0d0*ss2*(sz13-sz11)
      sl2 =  -2.0d0*ss3*sz2
      sl3 =  -2.0d0*ss3*(sz3-sz1)
      sl4 =  -2.0d0*ss3*(-21.0d0-9.0d0*emsq)*zes
      sgh2=   2.0d0*ss4*sz32
      sgh3=   2.0d0*ss4*(sz33-sz31)
      sgh4= -18.0d0*ss4*zes
      sh2 =  -2.0d0*ss2*sz22
      sh3 =  -2.0d0*ss2*(sz23-sz21)

!
!     DO LUNAR TERMS
!
      ee2 =   2.0d0*s1*s6
      e3  =   2.0d0*s1*s7
      xi2 =   2.0d0*s2*z12
      xi3 =   2.0d0*s2*(z13-z11)
      xl2 =  -2.0d0*s3*z2
      xl3 =  -2.0d0*s3*(z3-z1)
      xl4 =  -2.0d0*s3*(-21.0d0-9.0d0*emsq)*zel
      xgh2=   2.0d0*s4*z32
      xgh3=   2.0d0*s4*(z33-z31)
      xgh4= -18.0d0*s4*zel
      xh2 =  -2.0d0*s2*z22
      xh3 =  -2.0d0*s2*(z23-z21)

      call chkout ( 'ZZDSCM' )

      return
      end
