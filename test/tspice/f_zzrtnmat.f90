!$Procedure      F_ZZRTNMAT ( Test ZZRTNMAT )

      SUBROUTINE F_ZZRTNMAT ( OK )

! $ Abstract
!
!     Test the private rectangular to RTN transformation routine
!     ZZRTNMAT.
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
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This family tests the private rectangular to RTN transformation
!     routine ZZRTNMAT.
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
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 12-MAY-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      PI

!
!     Local parameters
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

!
!     Small delta value used for near 0 and pi
!     latitudes.
!
      DOUBLE PRECISION      EPSLAT
      PARAMETER           ( EPSLAT = 1.D-1 )


!
!     Numbers of latitude and longitude samples.
!
      INTEGER               NLATSM
      PARAMETER           ( NLATSM = 5 )

      INTEGER               NLONSM
      PARAMETER           ( NLONSM = 8 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      DELTLA
      DOUBLE PRECISION      DELTLO
      DOUBLE PRECISION      M      ( 3, 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      XM     ( 3, 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               NLAT
      INTEGER               NLON

!
!     Save everything.
!
      SAVE

!
!     Initial values
!
      DATA                  Z / 0.D0, 0.D0, 1.D0 /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZRTNMAT' )


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Input position is on the Z axis' )

      CALL ZZRTNMAT ( Z, M )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

!
!     We're going to loop over a variety of state vectors
!     and all coordinate system/coordinate combinations.
!     For each test, the derivative signs we find must match
!     those of the corresponding coordinate derivative.
!

!
!     DELTLA and DELTLO are, respectively the latitude and longitude
!     increments we use to create different direction vectors.
!
      DELTLA = PI()   / ( NLATSM - 1 )
      DELTLO = 2*PI() /   NLONSM


      DO NLON = 1, NLONSM

         DO NLAT = 1, NLATSM
!
!           Create the latitudinal coordinates of a position vector.
!
            LON = (NLON-1)*DELTLO

            LAT = PI()/2 - (NLAT-1)*DELTLA

!
!           Don't pick a singular latitude value.
!
            LAT = MIN ( LAT, PI()/2 - EPSLAT )
            LAT = MAX ( LAT, EPSLAT - PI()/2 )

!
!           Fill in the position portion of the state vector.
!
            CALL LATREC ( 1.D0, LON, LAT, V )

!
!---- Case -------------------------------------------------------------
!
            TITLE = 'lat #(deg); lon #(deg); '

            CALL REPMD ( TITLE, '#', DPR()*LAT,   3, TITLE )
            CALL REPMD ( TITLE, '#', DPR()*LON,   3, TITLE )

            CALL TCASE ( TITLE )

!
!           Construct the expected matrix.
!
            CALL TWOVEC ( V, 1, Z, 3, XM )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Construct the RTN matrix.
!
            CALL ZZRTNMAT ( V, M )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( 'M', M, '~~/', XM, 9, TIGHT, OK )

         END DO
!
!        End of latitude loop.
!
      END DO
!
!     End of longitude loop.
!

      CALL T_SUCCESS ( OK )
      RETURN
      END




