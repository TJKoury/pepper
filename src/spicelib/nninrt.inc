! $ Abstract
!
!     This file contains the number of non-inertial reference
!     frames that are currently built into the SPICE toolkit
!     software.
!
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
!     FRAMES
!
! $ Declarations

      INTEGER               NNINRT
      PARAMETER           ( NNINRT = 124 )


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
