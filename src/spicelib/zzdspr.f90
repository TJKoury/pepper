!$Procedure ZZDSPR ( SGP4 deep space long period )

      subroutine zzdspr (  opmode, &
                           e3,   ee2,   peo,   pgho,  pho,  pinco, &
                           plo,  se2,   se3,   sgh2,  sgh3, sgh4, &
                           sh2,  sh3,   si2,   si3,   sl2,  sl3, &
                           sl4,  t,     xgh2,  xgh3,  xgh4, xh2, &
                           xh3,  xi2,   xi3,   xl2,   xl3,  xl4, &
                           zmol, zmos,  inclo, doinit, &
                           eccp, inclp, nodep, argpp, mp)
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Provide deep space long period periodic contributions to the mean
!     elements. By design, these periodics are zero at epoch. This used
!     to be dscom which included initialization, but it's really a
!     recurring function.
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

      integer(ip)           opmode
      real(wp)              e3
      real(wp)              ee2
      real(wp)              peo
      real(wp)              pgho
      real(wp)              pho
      real(wp)              pinco
      real(wp)              plo
      real(wp)              se2
      real(wp)              se3
      real(wp)              sgh2
      real(wp)              sgh3
      real(wp)              sgh4
      real(wp)              sh2
      real(wp)              sh3
      real(wp)              si2
      real(wp)              si3
      real(wp)              sl2
      real(wp)              sl3
      real(wp)              sl4
      real(wp)              t
      real(wp)              xgh2
      real(wp)              xgh3
      real(wp)              xgh4
      real(wp)              xh2
      real(wp)              xh3
      real(wp)              xi2
      real(wp)              xi3
      real(wp)              xl2
      real(wp)              xl3
      real(wp)              xl4
      real(wp)              zmol
      real(wp)              zmos
      real(wp)              inclo
      logical               doinit
      real(wp)              eccp
      real(wp)              inclp
      real(wp)              nodep
      real(wp)              argpp
      real(wp)              mp

! $ Brief_I/O
!
!    See Detailed_input and Detailed_Output.
!
! $ Detailed_Input
!
!     OPMODE     Flag indicating which technique
!                to use to calculate sidereal time.
!
!     E3         Internal SGD4 parameter.
!
!     EE2        Internal SGD4 parameter.
!
!     PEO        Internal SGD4 parameter.
!
!     PGHO       Internal SGD4 parameter.
!
!     PHO        Internal SGD4 parameter.
!
!     PINCO      Internal SGD4 parameter.
!
!     PLO        Internal SGD4 parameter.
!
!     SE2        Internal SGD4 parameter.
!
!     SE3        Internal SGD4 parameter.
!
!     SGH2       Internal SGD4 parameter.
!
!     SGH3       Internal SGD4 parameter.
!
!     SGH4       Internal SGD4 parameter.
!
!     SH2        Internal SGD4 parameter.
!
!     SH3        Internal SGD4 parameter.
!
!     SI2        Internal SGD4 parameter.
!
!     SI3        Internal SGD4 parameter.
!
!     SL2        Internal SGD4 parameter.
!
!     SL3        Internal SGD4 parameter.
!
!     SL4        Internal SGD4 parameter.
!
!     T          Time for state evaluation.
!
!     XGH2       Internal SGD4 parameter.
!
!     XGH3       Internal SGD4 parameter.
!
!     XGH4       Internal SGD4 parameter.
!
!     XH2        Internal SGD4 parameter.
!
!     XH3        Internal SGD4 parameter.
!
!     XI2        Internal SGD4 parameter.
!
!     XI3        Internal SGD4 parameter.
!
!     XL2        Internal SGD4 parameter.
!
!     XL3        Internal SGD4 parameter.
!
!     XL4        Internal SGD4 parameter.
!
!     ZMOL       Internal SGD4 parameter.
!
!     ZMOS       Internal SGD4 parameter.
!
!     INCLO      Unused argument. Maintained in call for historical
!                reference.
!
!     DOINIT     Flag indicating initialization state. True to
!                initialize, false otherwise.
!
!     ECCP       Eccentricity.
!
!     INCLP      Inclination.
!
!     NODEP      Right ascension of  ascending node.
!
!     ARGPP      Argument of periapsis.
!
!     MP         Mean anomaly.
!
! $ Detailed_Output
!
!     ECCP       Calculated perturbed eccentricity.
!
!     INCLP      Calculated perturbed inclination.
!
!     NODEP      Calculated perturbed right ascension of
!                ascending node.
!
!     ARGPP      Calculated perturbed argument of periapsis.
!
!     MP         Calculated perturbed mean anomaly.
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
!     This routine is based on the DPPER code by David Vallado
!     corresponding to "Revisiting Spacetrack Report #3" [4].
!     The intent is to maintain the original Vallado algorithm,
!     changing code only to meet NAIF format standards and to
!     integrate with SPICELIB.
!
!        Capitalize all variables.
!
!        ENDIF replaced with END IF.
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
!        Tweaked wording of header Abstract section.
!        Fixed typos in comments.
!
!-    SPICELIB Version 1.0.0, SEP-15-2014 (EDW)
!
!        Based on routine DDPER, 28-JUN-2005, Vallado 2006 [4].
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
      real(wp)              alfdp
      real(wp)              betdp
      real(wp)              cosip
      real(wp)              cosop
      real(wp)              dalf
      real(wp)              dbet
      real(wp)              dls
      real(wp)              f2
      real(wp)              f3
      real(wp)              pe
      real(wp)              pgh
      real(wp)              ph
      real(wp)              pinc
      real(wp)              pl
      real(wp)              sel
      real(wp)              ses
      real(wp)              sghl
      real(wp)              sghs
      real(wp)              shl
      real(wp)              shs
      real(wp)              sil
      real(wp)              sinip
      real(wp)              sinop
      real(wp)              sinzf
      real(wp)              sis
      real(wp)              sll
      real(wp)              sls
      real(wp)              xls
      real(wp)              xnoh
      real(wp)              zel
      real(wp)              zes
      real(wp)              zf
      real(wp)              zm
      real(wp)              znl
      real(wp)              zns

!
!     SPICELIB routines.
!
      real(wp)                 twopi
      real(wp)                 pi
      logical                  return

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Local constants.
!
      zes  = 0.01675_wp
      zel  = 0.05490_wp
      zns  = 1.19459e-5_wp
      znl  = 1.5835218e-4_wp

!
!     Calculate time varying periodics.
!
      zm   = zmos + zns*t

      if (doinit) zm = zmos

      zf   = zm + 2.0_wp*zes*dsin(zm)
      sinzf= dsin(zf)
      f2   =  0.5_wp*sinzf*sinzf - 0.25_wp
      f3   = -0.5_wp*sinzf*dcos(zf)
      ses  = se2*f2 + se3*f3
      sis  = si2*f2 + si3*f3
      sls  = sl2*f2 + sl3*f3 + sl4*sinzf
      sghs = sgh2*f2 + sgh3*f3 + sgh4*sinzf
      shs  = sh2*f2 + sh3*f3
      zm   = zmol + znl*t

      if (doinit) zm = zmol

      zf   = zm + 2.0_wp*zel*dsin(zm)
      sinzf= dsin(zf)
      f2   =  0.5_wp*sinzf*sinzf - 0.25_wp
      f3   = -0.5_wp*sinzf*dcos(zf)
      sel  = ee2*f2 + e3*f3
      sil  = xi2*f2 + xi3*f3
      sll  = xl2*f2 + xl3*f3 + xl4*sinzf
      sghl = xgh2*f2 + xgh3*f3 + xgh4*sinzf
      shl  = xh2*f2 + xh3*f3
      pe   = ses + sel
      pinc = sis + sil
      pl   = sls + sll
      pgh  = sghs + sghl
      ph   = shs + shl

      if ( .not. doinit) then

         pe    = pe   - peo
         pinc  = pinc - pinco
         pl    = pl   - plo
         pgh   = pgh  - pgho
         ph    = ph   - pho
         inclp = inclp  + pinc
         eccp  = eccp   + pe
         sinip = dsin(inclp)
         cosip = dcos(inclp)

!
!        Apply periodics directly.
!
!        sgp4fix for lyddane choice
!
!        strn3 used original inclination - this is technically
!        feasible
!
!        gsfc used perturbed inclination - also technically feasible
!        probably best to readjust the 0.2 limit value and limit
!        discontinuity
!
!        0.2 rad = 11.45916 deg
!
!        use next line for original strn3 approach and original
!        inclination
!
!            IF (INCLO.GE.0.2D0) THEN
!
!        use next line for gsfc version and perturbed inclination
!
         if (inclp>=0.2_wp) then

            ph     = ph/sinip
            pgh    = pgh - cosip*ph
            argpp  = argpp + pgh
            nodep  = nodep + ph
            mp     = mp + pl

         else

!
!           Apply periodics with Lyddane modification.
!
            sinop  = dsin(nodep)
            cosop  = dcos(nodep)
            alfdp  = sinip*sinop
            betdp  = sinip*cosop
            dalf   =  ph*cosop + pinc*cosip*sinop
            dbet   = -ph*sinop + pinc*cosip*cosop
            alfdp  = alfdp + dalf
            betdp  = betdp + dbet
            nodep  = dmod(nodep,twopi())

!
!           sgp4fix for afspc written intrinsic functions
!           NODEP used without a trigonometric function ahead
!

            if ( (nodep  < 0.0_wp)      .and. &
                 (opmode == afspc)  )   then
               nodep = nodep + twopi()
            end if

            xls    = mp + argpp + cosip*nodep
            dls    = pl + pgh - pinc*nodep*sinip
            xls    = xls + dls
            xnoh   = nodep
            nodep  = datan2(alfdp,betdp)

!
!           sgp4fix for afspc written intrinsic functions
!           NODEP used without a trigonometric function ahead
!

            if ( (nodep < 0.0_wp)  .and. &
                 (opmode == afspc)  )  then
               nodep = nodep + twopi()
            end if

            if (dabs(xnoh-nodep) > pi()) then

               if (nodep < xnoh) then
                  nodep = nodep+twopi()
               else
                  nodep = nodep-twopi()
               end if

            end if

            mp   = mp + pl
            argpp=  xls - mp - cosip*nodep

         end if

      end if

      return
      end
