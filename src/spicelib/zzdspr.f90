!$Procedure ZZDSPR ( SGP4 deep space long period )

      SUBROUTINE ZZDSPR (  OPMODE, &
                           E3,   EE2,   PEO,   PGHO,  PHO,  PINCO, &
                           PLO,  SE2,   SE3,   SGH2,  SGH3, SGH4, &
                           SH2,  SH3,   SI2,   SI3,   SL2,  SL3, &
                           SL4,  T,     XGH2,  XGH3,  XGH4, XH2, &
                           XH3,  XI2,   XI3,   XL2,   XL3,  XL4, &
                           ZMOL, ZMOS,  INCLO, DOINIT, &
                           ECCP, INCLP, NODEP, ARGPP, MP)

! $ Abstract
!
!     Provide deep space long period periodic contributions to the mean
!     elements. By design, these periodics are zero at epoch. This used
!     to be dscom which included initialization, but it's really a
!     recurring function.
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

      INCLUDE 'zzsgp4.inc'

      INTEGER               OPMODE
      DOUBLE PRECISION      E3
      DOUBLE PRECISION      EE2
      DOUBLE PRECISION      PEO
      DOUBLE PRECISION      PGHO
      DOUBLE PRECISION      PHO
      DOUBLE PRECISION      PINCO
      DOUBLE PRECISION      PLO
      DOUBLE PRECISION      SE2
      DOUBLE PRECISION      SE3
      DOUBLE PRECISION      SGH2
      DOUBLE PRECISION      SGH3
      DOUBLE PRECISION      SGH4
      DOUBLE PRECISION      SH2
      DOUBLE PRECISION      SH3
      DOUBLE PRECISION      SI2
      DOUBLE PRECISION      SI3
      DOUBLE PRECISION      SL2
      DOUBLE PRECISION      SL3
      DOUBLE PRECISION      SL4
      DOUBLE PRECISION      T
      DOUBLE PRECISION      XGH2
      DOUBLE PRECISION      XGH3
      DOUBLE PRECISION      XGH4
      DOUBLE PRECISION      XH2
      DOUBLE PRECISION      XH3
      DOUBLE PRECISION      XI2
      DOUBLE PRECISION      XI3
      DOUBLE PRECISION      XL2
      DOUBLE PRECISION      XL3
      DOUBLE PRECISION      XL4
      DOUBLE PRECISION      ZMOL
      DOUBLE PRECISION      ZMOS
      DOUBLE PRECISION      INCLO
      LOGICAL               DOINIT
      DOUBLE PRECISION      ECCP
      DOUBLE PRECISION      INCLP
      DOUBLE PRECISION      NODEP
      DOUBLE PRECISION      ARGPP
      DOUBLE PRECISION      MP

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
      DOUBLE PRECISION      ALFDP
      DOUBLE PRECISION      BETDP
      DOUBLE PRECISION      COSIP
      DOUBLE PRECISION      COSOP
      DOUBLE PRECISION      DALF
      DOUBLE PRECISION      DBET
      DOUBLE PRECISION      DLS
      DOUBLE PRECISION      F2
      DOUBLE PRECISION      F3
      DOUBLE PRECISION      PE
      DOUBLE PRECISION      PGH
      DOUBLE PRECISION      PH
      DOUBLE PRECISION      PINC
      DOUBLE PRECISION      PL
      DOUBLE PRECISION      SEL
      DOUBLE PRECISION      SES
      DOUBLE PRECISION      SGHL
      DOUBLE PRECISION      SGHS
      DOUBLE PRECISION      SHL
      DOUBLE PRECISION      SHS
      DOUBLE PRECISION      SIL
      DOUBLE PRECISION      SINIP
      DOUBLE PRECISION      SINOP
      DOUBLE PRECISION      SINZF
      DOUBLE PRECISION      SIS
      DOUBLE PRECISION      SLL
      DOUBLE PRECISION      SLS
      DOUBLE PRECISION      XLS
      DOUBLE PRECISION      XNOH
      DOUBLE PRECISION      ZEL
      DOUBLE PRECISION      ZES
      DOUBLE PRECISION      ZF
      DOUBLE PRECISION      ZM
      DOUBLE PRECISION      ZNL
      DOUBLE PRECISION      ZNS

!
!     SPICELIB routines.
!
      DOUBLE PRECISION         TWOPI
      DOUBLE PRECISION         PI
      LOGICAL                  RETURN

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZDSPR' )

!
!     Local constants.
!
      ZES  = 0.01675D0
      ZEL  = 0.05490D0
      ZNS  = 1.19459D-5
      ZNL  = 1.5835218D-4

!
!     Calculate time varying periodics.
!
      ZM   = ZMOS + ZNS*T

      IF (DOINIT) ZM = ZMOS

      ZF   = ZM + 2.0D0*ZES*DSIN(ZM)
      SINZF= DSIN(ZF)
      F2   =  0.5D0*SINZF*SINZF - 0.25D0
      F3   = -0.5D0*SINZF*DCOS(ZF)
      SES  = SE2*F2 + SE3*F3
      SIS  = SI2*F2 + SI3*F3
      SLS  = SL2*F2 + SL3*F3 + SL4*SINZF
      SGHS = SGH2*F2 + SGH3*F3 + SGH4*SINZF
      SHS  = SH2*F2 + SH3*F3
      ZM   = ZMOL + ZNL*T

      IF (DOINIT) ZM = ZMOL

      ZF   = ZM + 2.0D0*ZEL*DSIN(ZM)
      SINZF= DSIN(ZF)
      F2   =  0.5D0*SINZF*SINZF - 0.25D0
      F3   = -0.5D0*SINZF*DCOS(ZF)
      SEL  = EE2*F2 + E3*F3
      SIL  = XI2*F2 + XI3*F3
      SLL  = XL2*F2 + XL3*F3 + XL4*SINZF
      SGHL = XGH2*F2 + XGH3*F3 + XGH4*SINZF
      SHL  = XH2*F2 + XH3*F3
      PE   = SES + SEL
      PINC = SIS + SIL
      PL   = SLS + SLL
      PGH  = SGHS + SGHL
      PH   = SHS + SHL

      IF ( .NOT. DOINIT) THEN

         PE    = PE   - PEO
         PINC  = PINC - PINCO
         PL    = PL   - PLO
         PGH   = PGH  - PGHO
         PH    = PH   - PHO
         INCLP = INCLP  + PINC
         ECCP  = ECCP   + PE
         SINIP = DSIN(INCLP)
         COSIP = DCOS(INCLP)

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
         IF (INCLP.GE.0.2D0) THEN

            PH     = PH/SINIP
            PGH    = PGH - COSIP*PH
            ARGPP  = ARGPP + PGH
            NODEP  = NODEP + PH
            MP     = MP + PL

         ELSE

!
!           Apply periodics with Lyddane modification.
!
            SINOP  = DSIN(NODEP)
            COSOP  = DCOS(NODEP)
            ALFDP  = SINIP*SINOP
            BETDP  = SINIP*COSOP
            DALF   =  PH*COSOP + PINC*COSIP*SINOP
            DBET   = -PH*SINOP + PINC*COSIP*COSOP
            ALFDP  = ALFDP + DALF
            BETDP  = BETDP + DBET
            NODEP  = DMOD(NODEP,TWOPI())

!
!           sgp4fix for afspc written intrinsic functions
!           NODEP used without a trigonometric function ahead
!

            IF ( (NODEP  .LT. 0.0D0)      .AND. &
                 (OPMODE .EQ. AFSPC)  )   THEN
               NODEP = NODEP + TWOPI()
            END IF

            XLS    = MP + ARGPP + COSIP*NODEP
            DLS    = PL + PGH - PINC*NODEP*SINIP
            XLS    = XLS + DLS
            XNOH   = NODEP
            NODEP  = DATAN2(ALFDP,BETDP)

!
!           sgp4fix for afspc written intrinsic functions
!           NODEP used without a trigonometric function ahead
!

            IF ( (NODEP .LT. 0.0D0)  .AND. &
                 (OPMODE .EQ. AFSPC)  )  THEN
               NODEP = NODEP + TWOPI()
            END IF

            IF (DABS(XNOH-NODEP) .GT. PI()) THEN

               IF (NODEP .LT. XNOH) THEN
                  NODEP = NODEP+TWOPI()
               ELSE
                  NODEP = NODEP-TWOPI()
               END IF

            END IF

            MP   = MP + PL
            ARGPP=  XLS - MP - COSIP*NODEP

         END IF

      END IF

      CALL CHKOUT ( 'ZZDSPR' )

      RETURN
      END
