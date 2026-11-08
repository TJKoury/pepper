C$Procedure F_PLN ( SPICE plane constructor/decomposition tests )
 
      SUBROUTINE F_PLN ( OK )
 
C$ Abstract
C
C     Exercise routines that construct SPICELIB planes or
C     decompose them.
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
C$ Files
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
C$ Particulars
C
C     This routine tests the SPICELIB plane construction and
C     decomposition routines.  The routines exercised by this family
C     are:
C
C        NVC2PL
C        NVP2PL
C        PL2NVC
C        PL2NVP
C        PL2PSV
C        PSV2PL
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 1.0.0, 20-SEP-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

C
C     Local Parameters
C
      DOUBLE PRECISION      LIMIT
      PARAMETER           ( LIMIT  = 1.D-12 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NUMCAS
      PARAMETER           ( NUMCAS = 10 )
 
      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )



C
C     Local Variables
C
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      CON
      DOUBLE PRECISION      CONST  (    NUMCAS )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      NHAT   (         3 )
      DOUBLE PRECISION      NMAG
      DOUBLE PRECISION      NORM   (         3 )
      DOUBLE PRECISION      NORMAL ( 3, NUMCAS )
      DOUBLE PRECISION      PLANE  (      UBPL )
      DOUBLE PRECISION      POINT  (         3 )
      DOUBLE PRECISION      POINT2 (         3 )
      DOUBLE PRECISION      SCLCON
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      V1     (         3 )
      DOUBLE PRECISION      V2     (         3 )
      
      INTEGER               CASE
      

C
C     Saved values
C
      SAVE                  CONST
      SAVE                  NORMAL

C
C     Initial values
C
      DATA NORMAL /  1.0D0,      0.D0,       0.D0,
     .               0.D0,       1.D0,       0.D0,
     .               0.D0,       0.D0,       1.D0,
     .               1.D20,      2.D20,      3.D20,
     .              -1.D20,      1.D10,      1.D5,
     .              -1.D5,      -1.D10,      1.D20,
     .              -1.D10,     -1.D5,      -1.D20,
     .              -1.D-20,     1.D-10,    -1.D-5,
     .               1.D-5,     -1.D-20,    -1.D-10,
     .               1.D8,       1.D-8,     -1.D-8     /

      DATA CONST  /  -1.D35,
     .                4.D-35,
     .                2.0D0,
     .                1.D20,
     .                0.D0,
     .               -1.D0,
     .                1.D0,
     .                1.D-10,
     .                1.D-20,
     .               -1.D0      /



C
C     Open the test family.
C
      CALL TOPEN ( 'F_PLN' )


      DO CASE = 1, NUMCAS
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Pass a plane around and see if it comes back ' //
     .           'unchanged; case #.'

         CALL REPMI ( TITLE, '#', CASE, TITLE )      
         CALL TCASE ( TITLE )

         CALL NVC2PL ( NORMAL(1,CASE), CONST(CASE), PLANE ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        The stored constant must be non-negative.
C
         CALL PL2NVC ( PLANE, NORM, CON )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         CALL CHCKSD ( 'CON', CON, '>=', 0.D0, 0.D0, OK )

 
         CALL PL2PSV ( PLANE, POINT, V1, V2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        The returned point should be the closest one to the origin.
C        Check against SCLCON below.
C
         DIST = VNORM (POINT)
         
C
C        While we've got the plane in this form, let's perturb POINT.
C        The perturbation shouldn't be too huge compared to POINT, or
C        we'll blow away the accuracy of POINT.
C
         CALL UNORM ( NORMAL(1,CASE), NHAT, NMAG )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         SCLCON =  CONST(CASE) / NMAG

         CALL VLCOM3 ( 1.D3 * SCLCON,  V1,
     .                 1.D3 * SCLCON,  V2,
     .                 1.D0,           POINT,  POINT2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VEQU ( POINT2, POINT )
     
C
C        Test DIST while we're at it.
C     
         IF ( DIST .NE. 0.D0 ) THEN         

            CALL CHCKSD ( 'DIST', DIST, '~/', ABS(SCLCON), LIMIT, OK )
                  
         END IF

C
C        Ok, keep going. 
C         
         CALL PSV2PL ( POINT, V1, V2, PLANE  )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        The stored constant must be non-negative.
C
         CALL PL2NVC ( PLANE, NORM, CON )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         CALL CHCKSD ( 'CON', CON, '>=', 0.D0, 0.D0, OK )



         CALL PL2NVP ( PLANE, NORM, POINT )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        The returned point should be the closest one to the origin.
C        Check against SCLCON below.
C
         DIST = VNORM (POINT)
         
         IF ( DIST .NE. 0.D0 ) THEN         

            CALL CHCKSD ( 'DIST', DIST, '~/', ABS(SCLCON), LIMIT, OK )
                  
         END IF
         
         CALL NVP2PL ( NORM, POINT, PLANE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PL2NVC ( PLANE, NORM, CON )
         CALL CHCKXC ( .FALSE., ' ', OK )


C
C        CON must be non-negative.
C
         CALL CHCKSD ( 'CON', CON, '>=', 0.D0, 0.D0, OK )

C
C        The unit normal must be pretty close in length to 1.
C   
         CALL CHCKSD ( '||NORMAL||', VNORM(NORMAL), '~', 
     .                 1.D0,         LIMIT,         OK    )
         

C
C        Check out the relative error in the plane constant and the
C        angular separation of the original and final normal vectors.
C            
         SEP  = VSEP ( NORM, NHAT )
         IF ( CONST(CASE) .GE. 0.D0 ) THEN

            CALL CHCKSD ( 'CON', CON, '~/', SCLCON, LIMIT, OK )
            CALL CHCKSD ( 'SEP', SEP, '~',  0.D0,   LIMIT, OK )

         ELSE

            CALL CHCKSD ( '-CON', -CON, '~/', SCLCON, LIMIT, OK )
            CALL CHCKSD ( 'SEP',   SEP, '~',  PI(),   LIMIT, OK )

         END IF
         
         
         
      END DO




C
C     Now for some error handling tests.
C      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NVC2PL:  pass in zero normal vector' )
 
      CALL CLEARD ( 3, NORM )
      
      CALL NVC2PL ( NORM, 0.D0, PLANE )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NVP2PL:  pass in zero normal vector' )

      CALL NVP2PL ( NORM, POINT, PLANE )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )



      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'PSV2PL error case:  dependent spanning vectors.' )

      CALL PSV2PL ( POINT, V1, V1, PLANE )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )
     

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


