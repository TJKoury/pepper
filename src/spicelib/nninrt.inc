! $ Abstract
!
!     This file contains the number of non-inertial reference
!     frames that are currently built into the SPICE toolkit
!     software.
!
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations

      integer               nninrt
      parameter           ( nninrt = 124 )


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NINERT     P   Number of built-in non-inertial reference frames.
!
! $ Parameters
!
!     NINERT     is the number of built-in non-inertial reference
!                frames.  This value is needed by both  ZZFDAT, and
!                FRAMEX.
!
! $ Author_and_Institution
!
!     B.V. Semenov    (JPL)
!     W.L. Taber      (JPL)
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.7.0, 26-AUG-2021 (BVS)
!
!        Increased the number of non-inertial frames from 106 to 124
!        in order to accommodate the following PCK based frames:
!
!           IAU_52_EUROPA
!           IAU_NIX
!           IAU_HYDRA
!           IAU_RYUGU
!           IAU_ARROKOTH
!           IAU_DIDYMOS_BARYCENTER
!           IAU_DIDYMOS
!           IAU_DIMORPHOS
!           IAU_DONALDJOHANSON
!           IAU_EURYBATES
!           IAU_EURYBATES_BARYCENTER
!           IAU_QUETA
!           IAU_POLYMELE
!           IAU_LEUCUS
!           IAU_ORUS
!           IAU_PATROCLUS_BARYCENTER
!           IAU_PATROCLUS
!           IAU_MENOETIUS
!
!-    SPICELIB Version 1.6.0, 30-OCT-2014 (BVS)
!
!        Increased the number of non-inertial frames from 105 to 106
!        in order to accommodate the following PCK based frame:
!
!           IAU_BENNU
!
!-    SPICELIB Version 1.5.0, 11-OCT-2011 (BVS)
!
!        Increased the number of non-inertial frames from 100 to 105
!        in order to accommodate the following PCK based frames:
!
!           IAU_CERES
!           IAU_PALLAS
!           IAU_LUTETIA
!           IAU_DAVIDA
!           IAU_STEINS
!
!-    SPICELIB Version 1.4.0, 11-MAY-2010 (BVS)
!
!        Increased the number of non-inertial frames from 96 to 100
!        in order to accommodate the following PCK based frames:
!
!           IAU_BORRELLY
!           IAU_TEMPEL_1
!           IAU_VESTA
!           IAU_ITOKAWA
!
!-    SPICELIB Version 1.3.0, 12-DEC-2002 (BVS)
!
!        Increased the number of non-inertial frames from 85 to 96
!        in order to accommodate the following PCK based frames:
!
!           IAU_CALLIRRHOE
!           IAU_THEMISTO
!           IAU_MAGACLITE
!           IAU_TAYGETE
!           IAU_CHALDENE
!           IAU_HARPALYKE
!           IAU_KALYKE
!           IAU_IOCASTE
!           IAU_ERINOME
!           IAU_ISONOE
!           IAU_PRAXIDIKE
!
!-    SPICELIB Version 1.2.0, 02-AUG-2002 (FST)
!
!        Increased the number of non-inertial frames from 81 to 85
!        in order to accommodate the following PCK based frames:
!
!           IAU_PAN
!           IAU_GASPRA
!           IAU_IDA
!           IAU_EROS
!
!-    SPICELIB Version 1.1.0, 20-FEB-1997 (WLT)
!
!        Increased the number of non-inertial frames from 79 to 81
!        in order to accommodate the following earth rotation
!        models:
!
!           ITRF93
!           EARTH_FIXED
!
!-    SPICELIB Version 1.0.0, 10-OCT-1996 (WLT)
!
!-&
