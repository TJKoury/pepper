!$Procedure ZZSGP4 ( SGP4 parameters )
!
! $ Abstract
!
!      Parameter assignments for SGP4 algorithm as expressed
!      by Vallado [2].
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
!
!     None.
!
! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
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
!     None.
!
! $ Examples
!
!     J2    = GEOPHS(K_J2)
!     J3    = GEOPHS(K_J3)
!     J4    = GEOPHS(K_J4)
!     ER    = GEOPHS(K_ER)
!     XKE   = GEOPHS(K_KE)
!
!     TUMIN = 1.D0/XKE
!     J3OJ2 = J3/J2
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
!   [2] Vallado, David, Crawford, Paul, Hujsak, Richard, and Kelso, T.S.
!       2006. Revisiting Spacetrack Report #3. Paper AIAA 2006-6753
!       presented at the AIAA/AAS Astrodynamics Specialist Conference,
!       August 21-24, 2006. Keystone, CO.
!
! $ Author_and_Institution
!
!     E. D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, MAY-27-2020 (EDW)
!
!        Updated descriptions of GEOPHS constants to be consistent
!        with what's used in other routines.
!
!-    SPICELIB Version 1.0.0 22-JUL-2014 (EDW)
!
!-&

! $ Index_Entries
!
!  SGP4
!
!-&


!
!      WGS gravitational constants IDs.
!

      INTEGER                       NGRAVS
      PARAMETER                   ( NGRAVS = 3 )

      INTEGER                       NGRAVC
      PARAMETER                   ( NGRAVC = 8 )


      INTEGER                       WGS721
      PARAMETER                   ( WGS721 = 1 )

      INTEGER                       WGS72
      PARAMETER                   ( WGS72  = 2 )

      INTEGER                       WGS84
      PARAMETER                   ( WGS84  = 3 )


!
!      Gravitational constant indices.
!

      INTEGER                       P_RAD
      PARAMETER                   ( P_RAD =  1 )

      INTEGER                       P_XKE
      PARAMETER                   ( P_XKE =  2 )

      INTEGER                       P_MU
      PARAMETER                   ( P_MU =   3 )

      INTEGER                       P_TUMN
      PARAMETER                   ( P_TUMN = 4 )

      INTEGER                       P_J2
      PARAMETER                   ( P_J2 =   5 )

      INTEGER                       P_J3
      PARAMETER                   ( P_J3 =   6 )

      INTEGER                       P_J4
      PARAMETER                   ( P_J4 =   7 )

      INTEGER                       P_J3J2
      PARAMETER                   ( P_J3J2 = 8 )


!
!     The following parameters give the indices in the GEOPHS
!     array of the various geophysical parameters needed for
!     the two line element sets.
!
!     K_J2  --- index of J2 gravitational harmonic for earth
!     K_J3  --- index of J3 gravitational harmonic for earth
!     K_J4  --- index of J4 gravitational harmonic for earth
!     K_KE  --- index of KE = sqrt(GM) in earth-radii**1.5/MIN
!     K_QO  --- index of high altitude bound for atmospheric
!               model in km
!     K_SO  --- index of low altitude bound for atmospheric
!               model in km
!     K_ER  --- index of earth equatorial radius in km
!     K_AE  --- index of distance units/earth radius
!

      INTEGER                       K_J2
      PARAMETER                   ( K_J2 = 1 )

      INTEGER                       K_J3
      PARAMETER                   ( K_J3 = 2 )

      INTEGER                       K_J4
      PARAMETER                   ( K_J4 = 3 )

      INTEGER                       K_KE
      PARAMETER                   ( K_KE = 4 )

      INTEGER                       K_QO
      PARAMETER                   ( K_QO = 5 )

      INTEGER                       K_SO
      PARAMETER                   ( K_SO = 6 )

      INTEGER                       K_ER
      PARAMETER                   ( K_ER = 7 )

      INTEGER                       K_AE
      PARAMETER                   ( K_AE = 8 )

      INTEGER                       NGEO
      PARAMETER                   ( NGEO = K_AE )

!
!     Operation mode values, OPMODE.
!

      INTEGER                       AFSPC
      PARAMETER                   ( AFSPC  = 1 )

      INTEGER                       IMPRVD
      PARAMETER                   ( IMPRVD = 2 )

!
!     An enumeration of the various components of the
!     elements array---ELEMS
!
!     KNDT20  --- location of NDT20
!     KNDD60  --- location of NDD60
!     KBSTAR  --- location of BSTAR
!     KINCL   --- location of INCL
!     KNODE0  --- location of NODE0
!     KECC    --- location of ECC
!     KOMEGA  --- location of OMEGA
!     KMO     --- location of MO
!     KNO     --- location of NO
!

      INTEGER                       KNDT20
      PARAMETER                   ( KNDT20 = 1 )

      INTEGER                       KNDD60
      PARAMETER                   ( KNDD60 = 2 )

      INTEGER                       KBSTAR
      PARAMETER                   ( KBSTAR = 3 )

      INTEGER                       KINCL
      PARAMETER                   ( KINCL  = 4 )

      INTEGER                       KNODE0
      PARAMETER                   ( KNODE0 = 5 )

      INTEGER                       KECC
      PARAMETER                   ( KECC   = 6 )

      INTEGER                       KOMEGA
      PARAMETER                   ( KOMEGA = 7 )

      INTEGER                       KMO
      PARAMETER                   ( KMO    = 8 )

      INTEGER                       KNO
      PARAMETER                   ( KNO    = 9 )

      INTEGER                       KEPOCH
      PARAMETER                   ( KEPOCH = 10 )

      INTEGER                       NELEMS
      PARAMETER                   ( NELEMS = KEPOCH )


