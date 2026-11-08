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
      use pepper_kinds, only: ip, wp

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

      real(wp)                 epoch
      real(wp)                 eccp
      real(wp)                 argpp
      real(wp)                 tc
      real(wp)                 inclp
      real(wp)                 nodep
      real(wp)                 np
      real(wp)                 snodm
      real(wp)                 cnodm
      real(wp)                 sinim
      real(wp)                 cosim
      real(wp)                 sinomm
      real(wp)                 cosomm
      real(wp)                 day
      real(wp)                 e3
      real(wp)                 ee2
      real(wp)                 eccm
      real(wp)                 emsq
      real(wp)                 gam
      real(wp)                 rtemsq
      real(wp)                 se2
      real(wp)                 peo
      real(wp)                 pgho
      real(wp)                 pho
      real(wp)                 pinco
      real(wp)                 plo
      real(wp)                 se3
      real(wp)                 sgh2
      real(wp)                 sgh3
      real(wp)                 sgh4
      real(wp)                 sh2
      real(wp)                 sh3
      real(wp)                 si2
      real(wp)                 si3
      real(wp)                 sl2
      real(wp)                 sl3
      real(wp)                 sl4
      real(wp)                 s1
      real(wp)                 s2
      real(wp)                 s3
      real(wp)                 s4
      real(wp)                 s5
      real(wp)                 s6
      real(wp)                 s7
      real(wp)                 ss1
      real(wp)                 ss2
      real(wp)                 ss3
      real(wp)                 ss4
      real(wp)                 ss5
      real(wp)                 ss6
      real(wp)                 ss7
      real(wp)                 sz1
      real(wp)                 sz2
      real(wp)                 sz3
      real(wp)                 sz11
      real(wp)                 sz12
      real(wp)                 sz13
      real(wp)                 sz21
      real(wp)                 sz22
      real(wp)                 sz23
      real(wp)                 sz31
      real(wp)                 sz32
      real(wp)                 sz33
      real(wp)                 xgh2
      real(wp)                 xgh3
      real(wp)                 xgh4
      real(wp)                 xh2
      real(wp)                 xh3
      real(wp)                 xi2
      real(wp)                 xi3
      real(wp)                 xl2
      real(wp)                 xl3
      real(wp)                 xl4
      real(wp)                 xn
      real(wp)                 z1
      real(wp)                 z2
      real(wp)                 z3
      real(wp)                 z11
      real(wp)                 z12
      real(wp)                 z13
      real(wp)                 z21
      real(wp)                 z22
      real(wp)                 z23
      real(wp)                 z31
      real(wp)                 z32
      real(wp)                 z33
      real(wp)                 zmol
      real(wp)                 zmos

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
      real(wp)                 zes
      real(wp)                 a1
      real(wp)                 a10
      real(wp)                 a2
      real(wp)                 a3
      real(wp)                 a4
      real(wp)                 a5
      real(wp)                 a6
      real(wp)                 a7
      real(wp)                 a8
      real(wp)                 a9
      real(wp)                 betasq
      real(wp)                 c1l
      real(wp)                 c1ss
      real(wp)                 cc
      real(wp)                 ctem
      real(wp)                 stem
      real(wp)                 x1
      real(wp)                 x2
      real(wp)                 x3
      real(wp)                 x4
      real(wp)                 x5
      real(wp)                 x6
      real(wp)                 x7
      real(wp)                 x8
      real(wp)                 xnodce
      real(wp)                 xnoi
      real(wp)                 zcosg
      real(wp)                 zcosgl
      real(wp)                 zcosgs
      real(wp)                 zcosh
      real(wp)                 zcoshl
      real(wp)                 zcosi
      real(wp)                 zcosil
      real(wp)                 zcosis
      real(wp)                 zel
      real(wp)                 zsing
      real(wp)                 zsingl
      real(wp)                 zsings
      real(wp)                 zsinh
      real(wp)                 zsinhl
      real(wp)                 zsini
      real(wp)                 zsinil
      real(wp)                 zsinis
      real(wp)                 zx
      real(wp)                 zy

      integer(ip)              lsflg

!
!     SPICELIB routines.
!
      real(wp)                 twopi
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
      zes    =  0.01675_wp
      zel    =  0.05490_wp
      c1ss   =  2.9864797e-6_wp
      c1l    =  4.7968065e-7_wp
      zsinis =  0.39785416_wp
      zcosis =  0.91744867_wp
      zcosgs =  0.1945905_wp
      zsings = -0.98088458_wp

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
      betasq = 1.0_wp-emsq
      rtemsq = dsqrt(betasq)

!
!     INITIALIZE LUNAR SOLAR TERMS
!
!     Note, EPOCH + 18261.5D0 corresponds to JD days since
!     1899-12-31 12:00:00.
!
      peo    = 0.0_wp
      pinco  = 0.0_wp
      plo    = 0.0_wp
      pgho   = 0.0_wp
      pho    = 0.0_wp
      day    = epoch + 18261.5_wp + tc/1440.0_wp
      xnodce = dmod(4.5236020_wp - 9.2422029e-4_wp*day,twopi())
      stem   = dsin(xnodce)
      ctem   = dcos(xnodce)
      zcosil = 0.91375164_wp - 0.03568096_wp*ctem
      zsinil = dsqrt(1.0_wp - zcosil*zcosil)
      zsinhl = 0.089683511_wp*stem / zsinil
      zcoshl = dsqrt(1.0_wp - zsinhl*zsinhl)
      gam    = 5.8351514_wp + 0.0019443680_wp*day
      zx     = 0.39785416_wp*stem/zsinil
      zy     = zcoshl*ctem + 0.91744867_wp*zsinhl*stem
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
      xnoi  = 1.0_wp / xn

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

         z31= 12.0_wp*x1*x1 - 3.0_wp*x3*x3
         z32= 24.0_wp*x1*x2 - 6.0_wp*x3*x4
         z33= 12.0_wp*x2*x2 - 3.0_wp*x4*x4
         z1 =  3.0_wp* (a1*a1 + a2*a2) + z31*emsq
         z2 =  6.0_wp* (a1*a3 + a2*a4) + z32*emsq
         z3 =  3.0_wp* (a3*a3 + a4*a4) + z33*emsq
         z11= -6.0_wp*a1*a5 + emsq* (-24.0_wp*x1*x7-6.0_wp*x3*x5)
         z12= -6.0_wp* (a1*a6 + a3*a5) + emsq* &
                 ( -24.0_wp*(x2*x7+x1*x8) - 6.0_wp*(x3*x6+x4*x5) )
         z13= -6.0_wp*a3*a6 + emsq*(-24.0_wp*x2*x8 - 6.0_wp*x4*x6)
         z21=  6.0_wp*a2*a5 + emsq*(24.0_wp*x1*x5-6.0_wp*x3*x7)
         z22=  6.0_wp* (a4*a5 + a2*a6) + emsq* &
                 (  24.0_wp*(x2*x5+x1*x6) - 6.0_wp*(x4*x7+x3*x8) )
         z23=  6.0_wp*a4*a6 + emsq*(24.0_wp*x2*x6 - 6.0_wp*x4*x8)
         z1 = z1 + z1 + betasq*z31
         z2 = z2 + z2 + betasq*z32
         z3 = z3 + z3 + betasq*z33
         s3 = cc*xnoi
         s2 = -0.5_wp*s3 / rtemsq
         s4 = s3*rtemsq
         s1 = -15.0_wp*eccm*s4
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

      zmol  = dmod( 4.7199672_wp + 0.22997150_wp*day-gam, twopi() )
      zmos  = dmod( 6.2565837_wp + 0.017201977_wp*day,    twopi() )

!
!     DO SOLAR TERMS
!
      se2 =   2.0_wp*ss1*ss6
      se3 =   2.0_wp*ss1*ss7
      si2 =   2.0_wp*ss2*sz12
      si3 =   2.0_wp*ss2*(sz13-sz11)
      sl2 =  -2.0_wp*ss3*sz2
      sl3 =  -2.0_wp*ss3*(sz3-sz1)
      sl4 =  -2.0_wp*ss3*(-21.0_wp-9.0_wp*emsq)*zes
      sgh2=   2.0_wp*ss4*sz32
      sgh3=   2.0_wp*ss4*(sz33-sz31)
      sgh4= -18.0_wp*ss4*zes
      sh2 =  -2.0_wp*ss2*sz22
      sh3 =  -2.0_wp*ss2*(sz23-sz21)

!
!     DO LUNAR TERMS
!
      ee2 =   2.0_wp*s1*s6
      e3  =   2.0_wp*s1*s7
      xi2 =   2.0_wp*s2*z12
      xi3 =   2.0_wp*s2*(z13-z11)
      xl2 =  -2.0_wp*s3*z2
      xl3 =  -2.0_wp*s3*(z3-z1)
      xl4 =  -2.0_wp*s3*(-21.0_wp-9.0_wp*emsq)*zel
      xgh2=   2.0_wp*s4*z32
      xgh3=   2.0_wp*s4*(z33-z31)
      xgh4= -18.0_wp*s4*zel
      xh2 =  -2.0_wp*s2*z22
      xh3 =  -2.0_wp*s2*(z23-z21)

      call chkout ( 'ZZDSCM' )

      return
      end
