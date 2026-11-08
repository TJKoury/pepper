C$Procedure      F_PGR ( Planetographic coordinate tests )

      SUBROUTINE F_PGR ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB planetographic coordinate
C     conversion and Jacobian routines.
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE

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
C     This routine tests the SPICELIB planetographic coordinate
C     and coordinate derivative conversion routines
C     
C        DPGRDR
C        DRDPGR
C        PGRREC
C        RECPGR
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 16-MAY-2014 (NJB)
C
C        Bug fix: this routine no longer modifies XLON.
C        Added full header.
C
C-    TSPICE Version 1.0.0, 05-JAN-2005 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI

      LOGICAL               EQSTR
      LOGICAL               VZERO
      LOGICAL               VZEROG

C
C     Local parameters
C
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK = 'f_recpgr.tpc' )


      DOUBLE PRECISION      TIGHT 
      PARAMETER           ( TIGHT  = 1.D-11 )

      DOUBLE PRECISION      VTIGHT 
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-10 )


      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               NBODS
      PARAMETER           ( NBODS  = 75 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXCOF
      PARAMETER           ( MAXCOF = 3 )

      INTEGER               NCASE
      PARAMETER           ( NCASE  = 18 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NSPEC
      PARAMETER           ( NSPEC  = 3 )

C
C     Local variables
C
      CHARACTER*(NAMLEN)    BODY   ( NCASE )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(NAMLEN)    SPCIAL ( NSPEC )
      
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      COEFFS ( MAXCOF )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      GALT
      DOUBLE PRECISION      GLAT
      DOUBLE PRECISION      GLON
      DOUBLE PRECISION      JAC2   ( 3, 3 )
      DOUBLE PRECISION      JACOBI ( 3, 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      REC    ( 3 ) 
      DOUBLE PRECISION      RECTAN ( 3, NCASE )
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      XALT   ( NCASE )
      DOUBLE PRECISION      XLAT   ( NCASE )
      DOUBLE PRECISION      XLON   ( NCASE )
      DOUBLE PRECISION      XREC   ( 3 )
      DOUBLE PRECISION      XTEMP

      INTEGER               I
      INTEGER               J
      INTEGER               N

C
C     Saved variables
C
      SAVE                  BODY
      SAVE                  RECTAN
      SAVE                  SPCIAL
      SAVE                  XALT
      SAVE                  XLAT
      SAVE                  XLON

C
C     Initial values
C
      DATA                ( BODY(I), I = 1, 9 )    / 
     .                                'EARTH',
     .                                'EARTH',
     .                                'EARTH',
     .                                'EARTH',
     .                                'EARTH',
     .                                'EARTH',
     .                                'EARTH',
     .                                'EARTH',
     .                                'EARTH' /

      DATA                ( BODY(I), I = 10, NCASE )    / 
     .                                'MARS',
     .                                'MARS',
     .                                'MARS',
     .                                'MARS',
     .                                'MARS',
     .                                'MARS',
     .                                'MARS',
     .                                'MARS',
     .                                'MARS' /

      DATA                ( ( RECTAN(J,I), J = 1, 3 ), I = 1, 9 )  /  
     .                            6378.140D0,     0.000D0,     0.000D0,
     .                           -6378.140D0,     0.000D0,     0.000D0,
     .                           -6388.140D0,     0.000D0,     0.000D0,
     .                           -6368.140D0,     0.000D0,     0.000D0,
     .                               0.000D0, -6378.140D0,     0.000D0,
     .                               0.000D0,  6378.140D0,     0.000D0,
     .                               0.000D0,     0.000D0,  6356.750D0,
     .                               0.000D0,     0.000D0, -6356.750D0,
     .                               0.000D0,     0.000D0,     0.000D0 
     .                              /

      DATA                ( ( RECTAN(J,I), J = 1, 3 ), I = 10, NCASE ) /
     .                            3397.000D0,     0.000D0,     0.000D0,
     .                           -3397.000D0,     0.000D0,     0.000D0,
     .                           -3407.000D0,     0.000D0,     0.000D0,
     .                           -3387.000D0,     0.000D0,     0.000D0,
     .                               0.000D0, -3397.000D0,     0.000D0,
     .                               0.000D0,  3397.000D0,     0.000D0,
     .                               0.000D0,     0.000D0,  3375.000D0,
     .                               0.000D0,     0.000D0, -3375.000D0,
     .                               0.000D0,     0.000D0,     0.000D0 
     .                              /

      DATA  ( XLON(I), XLAT(I), XALT(I), I = 1, 9 ) /
     .                               0.000D0,     0.000D0,      0.000D0,
     .                             180.000D0,     0.000D0,      0.000D0,
     .                             180.000D0,     0.000D0,     10.000D0,
     .                             180.000D0,     0.000D0,    -10.000D0,
     .                             270.000D0,     0.000D0,      0.000D0,
     .                              90.000D0,     0.000D0,      0.000D0,
     .                               0.000D0,    90.000D0,      0.000D0,
     .                               0.000D0,   -90.000D0,      0.000D0,
     .                               0.000D0,    90.000D0,  -6356.750D0
     .                                                  /

      DATA  ( XLON(I), XLAT(I), XALT(I), I = 10, NCASE ) /
     .                               0.000D0,     0.000D0,      0.000D0,
     .                             180.000D0,     0.000D0,      0.000D0,
     .                             180.000D0,     0.000D0,     10.000D0,
     .                             180.000D0,     0.000D0,    -10.000D0,
     .                              90.000D0,     0.000D0,      0.000D0,
     .                             270.000D0,     0.000D0,      0.000D0,
     .                               0.000D0,    90.000D0,      0.000D0,
     .                               0.000D0,   -90.000D0,      0.000D0,
     .                               0.000D0,    90.000D0,  -3375.000D0
     .                                                  /


      DATA  SPCIAL / 'EARTH', 'MOON', 'SUN' /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_PGR' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create full text PCK file.' )
C
C     Create, load, and delete a PCK kernel.
C
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Loop through the canned cases.
C
      DO I = 1, NCASE

         TITLE = 'Canned case #; BODY = #.'
         CALL REPMI ( TITLE, '#', I,       TITLE )
         CALL REPMC ( TITLE, '#', BODY(I), TITLE )

C
C --- Case: ------------------------------------------------------
C
         CALL TCASE ( TITLE )
         
         CALL BODVRD ( BODY(I), 'RADII', 3, N, RADII )
         
         RE = RADII(1)
         RP = RADII(3)

         F  =  ( RE - RP ) / RE

C
C        Test RECPGR.
C
         CALL RECPGR ( BODY(I), RECTAN(1,I), RE, F,  LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LON = DPR() * LON
         LAT = DPR() * LAT

         CALL CHCKSD ( 'LON',  LON, '~', XLON(I), VTIGHT, OK )
         CALL CHCKSD ( 'LAT',  LAT, '~', XLAT(I), VTIGHT, OK )
         CALL CHCKSD ( 'ALT',  ALT, '~', XALT(I),  TIGHT, OK )

C
C        Test PGRREC.
C
         CALL PGRREC ( BODY(I), RPD()*XLON(I), RPD()*XLAT(I), XALT(I),
     .                 RE,      F,             REC                     )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Test the vector relative error if the test point is not
C        at the origin.
C
         IF (  .NOT.  VZERO( RECTAN(1,I) )  ) THEN

            CALL CHCKAD( 'REC', REC, '~~/', RECTAN(1,I), 3, VTIGHT, OK )

         ELSE
C
C           Check component-wise differences.
C
            CALL CHCKAD( 'REC', REC, '~', RECTAN(1,I), 3, VTIGHT, OK )

         END IF

      END DO

C
C     Run a non-axis test case.   
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test non-axis case against RECGEO for oblate ' //
     .             'body.' )

      CALL VPACK ( 1.D5, 1.D5, 1.D5, XREC ) 

      
      CALL BODVRD ( 'Mars', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      RE = RADII(1)
      RP = RADII(3)

      F = ( RE - RP ) / RE

C
C     Test RECPGR.
C
      CALL RECPGR ( 'MARS', XREC, RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECGEO ( XREC, RE, F, GLON, GLAT, GALT )
      CALL CHCKXC ( .FALSE., ' ', OK )
     
C
C     Mars is prograde, so flip the sign of the longitude.
C
      GLON = - GLON

      IF ( GLON .LT. 0.D0 ) THEN
         GLON = GLON + TWOPI()
      END IF

      CALL CHCKSD ( 'LON',  LON, '~', GLON, VTIGHT, OK )
      CALL CHCKSD ( 'LAT',  LAT, '~', GLAT, VTIGHT, OK )
      CALL CHCKSD ( 'ALT',  ALT, '~', GALT,  TIGHT, OK )

C
C     Now test PGRREC.
C
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, REC )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL CHCKAD( 'REC', REC, '~~/', XREC, 3, VTIGHT, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Repeat previous test with Mars longitude ' //
     .             'set to positive east.'                     )


      CALL PCPOOL ( 'BODY499_PGR_POSITIVE_LON', 1, 'EAST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Test RECPGR.
C
      CALL RECPGR ( 'MARS', XREC, RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECGEO ( XREC, RE, F, GLON, GLAT, GALT )
      CALL CHCKXC ( .FALSE., ' ', OK )
     

      IF ( GLON .LT. 0.D0 ) THEN
         GLON = GLON + TWOPI()
      END IF
 
      CALL CHCKSD ( 'LON',  LON, '~', GLON, VTIGHT, OK )
      CALL CHCKSD ( 'LAT',  LAT, '~', GLAT, VTIGHT, OK )
      CALL CHCKSD ( 'ALT',  ALT, '~', GALT,  TIGHT, OK )

C
C     Now test PGRREC.
C
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, REC )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL CHCKAD( 'REC', REC, '~~/', XREC, 3, VTIGHT, OK )

C
C     Clear the override value from the pool.
C     
      CALL DVPOOL ( 'BODY499_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Repeat previous test with Mars PM values ' //
     .             'absent from the kernel pool.'             )

C
C     Capture the correct values before starting.
C     
      CALL BODVRD ( 'Mars', 'PM', 3, N, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY499_PM' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCPOOL ( 'BODY499_PGR_POSITIVE_LON', 1, 'EAST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Test RECPGR.
C
      CALL RECPGR ( 'MARS', XREC, RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECGEO ( XREC, RE, F, GLON, GLAT, GALT )
      CALL CHCKXC ( .FALSE., ' ', OK )
     

      IF ( GLON .LT. 0.D0 ) THEN
         GLON = GLON + TWOPI()
      END IF
 
      CALL CHCKSD ( 'LON',  LON, '~', GLON, VTIGHT, OK )
      CALL CHCKSD ( 'LAT',  LAT, '~', GLAT, VTIGHT, OK )
      CALL CHCKSD ( 'ALT',  ALT, '~', GALT,  TIGHT, OK )

C
C     Now test PGRREC.
C
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, REC )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL CHCKAD( 'REC', REC, '~~/', XREC, 3, VTIGHT, OK )

C
C     Clear the override value from the pool.
C     
      CALL DVPOOL ( 'BODY499_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Restore the Mars PM coefficients.
C
      CALL PDPOOL ( 'BODY499_PM', 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check longitude sense for earth, moon, and sun.' )


      DO I = 1, NSPEC
C
C        Test RECPGR.
C
         CALL RECPGR ( SPCIAL(I), XREC, RE, F, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECGEO ( XREC, RE, F, GLON, GLAT, GALT )
         CALL CHCKXC ( .FALSE., ' ', OK )
     

         IF ( GLON .LT. 0.D0 ) THEN
            GLON = GLON + TWOPI()
         END IF
 
         CALL CHCKSD ( 'LON',  LON, '~', GLON, VTIGHT, OK )
         CALL CHCKSD ( 'LAT',  LAT, '~', GLAT, VTIGHT, OK )
         CALL CHCKSD ( 'ALT',  ALT, '~', GALT,  TIGHT, OK )

C
C        Now test PGRREC.
C
         CALL PGRREC ( SPCIAL(I), LON, LAT, ALT, RE, F, REC )
         CALL CHCKXC ( .FALSE., ' ', OK )
      
         CALL CHCKAD( 'REC', REC, '~~/', XREC, 3, VTIGHT, OK )

      END DO



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Repeat tests with longitudes set to positive ' //
     .             'west.' )

      CALL PCPOOL ( 'BODY399_PGR_POSITIVE_LON', 1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCPOOL ( 'BODY301_PGR_POSITIVE_LON', 1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL PCPOOL ( 'BODY10_PGR_POSITIVE_LON',  1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NSPEC
C
C        Test RECPGR.
C
         CALL RECPGR ( SPCIAL(I), XREC, RE, F, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECGEO ( XREC, RE, F, GLON, GLAT, GALT )
         CALL CHCKXC ( .FALSE., ' ', OK )
     
         GLON = -GLON

         IF ( GLON .LT. 0.D0 ) THEN
            GLON = GLON + TWOPI()
         END IF
 
         CALL CHCKSD ( 'LON',  LON, '~', GLON, VTIGHT, OK )
         CALL CHCKSD ( 'LAT',  LAT, '~', GLAT, VTIGHT, OK )
         CALL CHCKSD ( 'ALT',  ALT, '~', GALT,  TIGHT, OK )

C
C        Now test PGRREC.
C
         CALL PGRREC ( SPCIAL(I), LON, LAT, ALT, RE, F, REC )
         CALL CHCKXC ( .FALSE., ' ', OK )
      
         CALL CHCKAD( 'REC', REC, '~~/', XREC, 3, VTIGHT, OK )

      END DO


      CALL DVPOOL ( 'BODY399_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY301_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY10_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Error cases.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: equatorial radius is zero.' )
      
C
C     Capture the correct values before starting.
C     
      CALL BODVRD ( 'Earth', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RE = RADII(1)
      RP = RADII(3)

      F = ( RE - RP ) / RE


      CALL RECPGR ( 'EARTH', RECTAN(1,1), 0.D0, F, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'EARTH', LON, LAT, ALT, 0.D0, F, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: flattening factor is 1.'   )

      CALL RECPGR ( 'EARTH', RECTAN(1,1), RE, 1.D0, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'EARTH', LON, LAT, ALT, RE, 1.D0, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: flattening factor is > 1.'   )

      CALL RECPGR ( 'EARTH', RECTAN(1,1), RE, 2.D0, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'EARTH', LON, LAT, ALT, RE, 2.D0, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no Mars PM terms in pool.'  )
C
C     Capture the correct values before starting.
C     
      CALL BODVRD ( 'Mars', 'PM', 3, N, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Delete Mars' PM coefficients from the kernel pool.
C
      CALL DVPOOL ( 'BODY499_PM' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECPGR ( 'MARS', RECTAN(1,1), RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no rate term for Mars PM.'  )
C
C     Overwrite Mars' PM coefficients in the kernel pool.
C
      CALL PDPOOL ( 'BODY499_PM', 1, 0.D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECPGR ( 'MARS', RECTAN(1,1), RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

C
C     Restore the good PM coefficients.
C
      CALL PDPOOL ( 'BODY499_PM', 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: invalid override spec.'  )


      CALL PCPOOL ( 'BODY499_PGR_POSITIVE_LON', 1, 'EST' )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL RECPGR ( 'MARS', RECTAN(1,1), RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C     Clean up.
C
      CALL DVPOOL ('BODY499_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )




C *****************************************************************
C *****************************************************************
C *****************************************************************
C     Jacobian matrix routine tests
C *****************************************************************
C *****************************************************************
C *****************************************************************



C
C     Loop through the canned cases.
C
      DO I = 1, NCASE

         TITLE = 'Canned DPGRDR case #; BODY = #.'
         CALL REPMI ( TITLE, '#', I,       TITLE )
         CALL REPMC ( TITLE, '#', BODY(I), TITLE )

C
C --- Case: ------------------------------------------------------
C
         CALL TCASE ( TITLE )

         CALL BODVRD ( BODY(I), 'RADII', 3, N, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )

         RE = RADII(1)
         RP = RADII(3)

         F  =  ( RE - RP ) / RE

         CALL DRDPGR ( BODY(I), 
     .                 XLON(I), XLAT(I), XALT(I), 
     .                 RE,      F,       JACOBI       )


         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( EQSTR( BODY(I), 'Mars' )  ) THEN
            XTEMP = -XLON(I)
         ELSE
            XTEMP =  XLON(I)
         END IF

         CALL DRDGEO ( XTEMP,   XLAT(I), XALT(I), 
     .                 RE,      F,       JAC2      )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        For comparison, negate the partials w.r.t. geodetic longitude
C        for Mars.
C 
         IF ( EQSTR( BODY(I), 'Mars' )  ) THEN

            DO J = 1, 3
               JAC2(J,1) =  -JAC2(J,1)
            END DO

         END IF

         CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )

      END DO

 
C
C     Loop through the canned cases for DRDPGR.
C
      DO I = 1, NCASE

         TITLE = 'Canned DRDPGR case #; BODY = #.'
         CALL REPMI ( TITLE, '#', I,       TITLE )
         CALL REPMC ( TITLE, '#', BODY(I), TITLE )

C
C --- Case: ------------------------------------------------------
C
         CALL TCASE ( TITLE )

         CALL BODVRD ( BODY(I), 'RADII', 3, N, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )

         RE = RADII(1)
         RP = RADII(3)

         F  =  ( RE - RP ) / RE

         CALL DPGRDR ( BODY(I), 
     .                 RECTAN(1,I), RECTAN(2,I), RECTAN(3,I), 
     .                 RE,          F,           JACOBI       )

         IF (   VZEROG ( RECTAN(1,I), 2 )  ) THEN

            CALL CHCKXC ( .TRUE., 'SPICE(POINTONZAXIS)', OK )

         ELSE

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DGEODR ( RECTAN(1,I), RECTAN(2,I), RECTAN(3,I), 
     .                    RE,          F,           JAC2         )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           For comparison, negate the gradient of geodetic longitude
C           for Mars.
 
            IF ( EQSTR( BODY(I), 'Mars' )  ) THEN

               DO J = 1, 3
                  JAC2(1,J) =  -JAC2(1,J)
               END DO

            END IF

            CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )

         END IF

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check longitude sense for earth, moon, and sun.' )


      CALL VPACK ( 1.D5, 1.D5, 1.D5, XREC ) 

      DO I = 1, NSPEC
C
C        Test DPGRDR.
C
         CALL DPGRDR ( SPCIAL(I), XREC(1), XREC(2), XREC(3), 
     .                 RE,        F,       JACOBI           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DGEODR (            XREC(1), XREC(2), XREC(3), 
     .                 RE,        F,       JAC2             )
         CALL CHCKXC ( .FALSE., ' ', OK )
      
         CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )
C
C        Now test DRDPGR.
C
         CALL DRDPGR ( SPCIAL(I), LON, LAT, ALT, RE, F, JACOBI )
         CALL CHCKXC ( .FALSE., ' ', OK )
      
         CALL DRDGEO (            LON, LAT, ALT, RE, F, JAC2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )
         

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Repeat tests with longitudes set to positive ' //
     .             'west.' )

      CALL PCPOOL ( 'BODY399_PGR_POSITIVE_LON', 1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCPOOL ( 'BODY301_PGR_POSITIVE_LON', 1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL PCPOOL ( 'BODY10_PGR_POSITIVE_LON',  1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NSPEC
C
C        Test DPGRDR.
C
         CALL DPGRDR ( SPCIAL(I), XREC(1), XREC(2), XREC(3), 
     .                 RE,        F,       JACOBI           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DGEODR (            XREC(1), XREC(2), XREC(3), 
     .                 RE,        F,       JAC2             )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Negate the gradient of geodetic longitude.
C
         DO J = 1, 3
            JAC2(1,J) = -JAC2(1,J)
         END DO
      
         CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )

C
C        Now test DRDPGR.
C
         CALL DRDPGR ( SPCIAL(I), LON, LAT, ALT, RE, F, JACOBI )
         CALL CHCKXC ( .FALSE., ' ', OK )
      
         CALL DRDGEO (            LON, LAT, ALT, RE, F, JAC2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Negate all partials with respect to geodetic longitude.
C
         DO J = 1, 3
            JAC2(J,1) = -JAC2(J,1)
         END DO

         CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )
         
      END DO


C
C     Error cases.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: equatorial radius is zero.' )
      
C
C     Capture the correct values before starting.
C     
      CALL BODVRD ( 'Earth', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RE = RADII(1)
      RP = RADII(3)

      F = ( RE - RP ) / RE


      CALL DPGRDR ( 'EARTH', 
     .               RECTAN(1,1), RECTAN(2,1), RECTAN(3,1),
     .               0.D0,        F,           JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'EARTH', LON, LAT, ALT, 0.D0, F, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: flattening factor is 1.'   )

      CALL DPGRDR ( 'EARTH', 
     .               RECTAN(1,1), RECTAN(2,1), RECTAN(3,1),
     .               RE,          1.D0,        JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'EARTH', LON, LAT, ALT, RE, 1.D0, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: flattening factor is > 1.'   )

      CALL DPGRDR ( 'EARTH', 
     .               RECTAN(1,1), RECTAN(2,1), RECTAN(3,1),
     .               RE,        2.D0,        JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'EARTH', LON, LAT, ALT, RE, 2.D0, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no Mars PM terms in pool.'  )
C
C     Capture the correct values before starting.
C     
      CALL BODVRD ( 'Mars', 'PM', 3, N, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Delete Mars' PM coefficients from the kernel pool.
C
      CALL DVPOOL ( 'BODY499_PM' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DPGRDR ( 'MARS', 
     .               RECTAN(1,1), RECTAN(2,1), RECTAN(3,1),
     .               RE,          F,           JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'MARS', LON, LAT, ALT, RE, F, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no rate term for Mars PM.'  )
C
C     Overwrite Mars' PM coefficients in the kernel pool.
C
      CALL PDPOOL ( 'BODY499_PM', 1, 0.D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DPGRDR ( 'MARS', 
     .               RECTAN(1,1), RECTAN(2,1), RECTAN(3,1),
     .               RE,          F,           JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'MARS', LON, LAT, ALT, RE, F, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

C
C     Restore the good PM coefficients.
C
      CALL PDPOOL ( 'BODY499_PM', 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: invalid override spec.'  )


      CALL PCPOOL ( 'BODY499_PGR_POSITIVE_LON', 1, 'EST' )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DPGRDR ( 'MARS', 
     .               RECTAN(1,1), RECTAN(2,1), RECTAN(3,1),
     .               RE,          F,           JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'MARS', LON, LAT, ALT, RE, F, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )



C
C     Clean up.
C
      CALL DVPOOL ('BODY499_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
