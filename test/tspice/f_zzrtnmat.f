C$Procedure      F_ZZRTNMAT ( Test ZZRTNMAT )
  
      SUBROUTINE F_ZZRTNMAT ( OK )

C$ Abstract
C
C     Test the private rectangular to RTN transformation routine
C     ZZRTNMAT.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This family tests the private rectangular to RTN transformation
C     routine ZZRTNMAT.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 12-MAY-2008 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      PI

C     
C     Local parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

C
C     Small delta value used for near 0 and pi 
C     latitudes.
C
      DOUBLE PRECISION      EPSLAT
      PARAMETER           ( EPSLAT = 1.D-1 )

      
C
C     Numbers of latitude and longitude samples.
C 
      INTEGER               NLATSM
      PARAMETER           ( NLATSM = 5 )

      INTEGER               NLONSM
      PARAMETER           ( NLONSM = 8 )

C
C     Local Variables
C
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

C
C     Save everything.
C
      SAVE
 
C
C     Initial values
C
      DATA                  Z / 0.D0, 0.D0, 1.D0 /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZRTNMAT' )


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Input position is on the Z axis' )

      CALL ZZRTNMAT ( Z, M )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************

C
C     We're going to loop over a variety of state vectors
C     and all coordinate system/coordinate combinations.
C     For each test, the derivative signs we find must match
C     those of the corresponding coordinate derivative.
C

C
C     DELTLA and DELTLO are, respectively the latitude and longitude
C     increments we use to create different direction vectors.
C
      DELTLA = PI()   / ( NLATSM - 1 )
      DELTLO = 2*PI() /   NLONSM


      DO NLON = 1, NLONSM

         DO NLAT = 1, NLATSM
C
C           Create the latitudinal coordinates of a position vector.
C
            LON = (NLON-1)*DELTLO

            LAT = PI()/2 - (NLAT-1)*DELTLA

C
C           Don't pick a singular latitude value.
C
            LAT = MIN ( LAT, PI()/2 - EPSLAT )
            LAT = MAX ( LAT, EPSLAT - PI()/2 )

C
C           Fill in the position portion of the state vector.
C
            CALL LATREC ( 1.D0, LON, LAT, V )

C
C---- Case -------------------------------------------------------------
C
            TITLE = 'lat #(deg); lon #(deg); '

            CALL REPMD ( TITLE, '#', DPR()*LAT,   3, TITLE )
            CALL REPMD ( TITLE, '#', DPR()*LON,   3, TITLE )

            CALL TCASE ( TITLE )

C
C           Construct the expected matrix.
C
            CALL TWOVEC ( V, 1, Z, 3, XM )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Construct the RTN matrix.
C
            CALL ZZRTNMAT ( V, M )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( 'M', M, '~~/', XM, 9, TIGHT, OK )
            
         END DO
C
C        End of latitude loop.
C
      END DO
C
C     End of longitude loop.
C

      CALL T_SUCCESS ( OK )
      RETURN
      END




