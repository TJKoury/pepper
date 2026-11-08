C$Procedure F_SAELGV ( SAELGV tests )
 
      SUBROUTINE F_SAELGV ( OK )
 
C$ Abstract
C
C     Exercise SAELGV: the SPICELIB routine that finds semi-axes
C     of an ellipse defined by two generating vectors.
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
C     This routine tests the SPICELIB routine SAELGV.  The
C     underlying routine DIAGS2 is exercised by these tests.
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
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD

C
C     Local Parameters
C
      DOUBLE PRECISION      EMAX
      PARAMETER           ( EMAX  = 306.D0 )

      DOUBLE PRECISION      LIMIT
      PARAMETER           ( LIMIT = 1.D-13 )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )

      INTEGER               UBEL
      PARAMETER           ( UBEL   =  9 )
      
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NCASE
      PARAMETER           ( NCASE  = 500 )
      
      INTEGER               NPT
      PARAMETER           ( NPT    = 10 )


C
C     Local Variables
C
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      EV1
      DOUBLE PRECISION      EV2
      DOUBLE PRECISION      EXPMAJ ( 3 )
      DOUBLE PRECISION      EXPMIN ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      VEC1   ( 3 )
      DOUBLE PRECISION      VEC2   ( 3 )

      INTEGER               J
      INTEGER               CASE
      INTEGER               SEED
     
C
C     Saved values
C
      SAVE                  CENTER

C
C     Initial values
C
      DATA                  CENTER / 3 * 0.D0   /



C
C     Open the test family.
C
      CALL TOPEN ( 'F_SAELGV' )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'A case that can be checked by inspection.'
      CALL TCASE  ( TITLE )

      VEC1(1) =  1.D0
      VEC1(2) =  1.D0
      VEC1(3) =  0.D0
      
      VEC2(1) =  1.D0
      VEC2(2) =  0.D0
      VEC2(3) =  0.D0

C
C     The equation of this ellipse is 
C     
C                 2       2
C        ( x - y )    +  y       =   1
C        
C     or
C     
C         2                   2
C        x    -   2xy   +   2y   =  1
C
C     The left hand side is
C     
C        ( x  y ) (  1   -1 ) ( x )
C                 ( -1    2 ) ( y )
C                 
C     The eigenvalues of the matrix are 
C     
C                       ___
C         3     +     \/ 5
C        ---    _     ------  
C         2              2
C         
C     Letting these be EV1 and EV2, some orthogonal eigenvectors are
C     
C        (    1    )     ( EV1 - 1 )
C        ( 1 - EV1 )     (    1    )
C        
C     A rotation to a basis formed from the eigenvectors gives
C     us the ellipse equation
C     
C              2             2
C        EV1  u    +    EV2 v    =   1
C
C     The semi-major axis length is the square root of 1/EV2;
C     The semi-minor axis length is the square root of 1/EV1.
C     


      EV1 =  ( 3.0D0 + DSQRT(5.D0) ) / 2.D0
      EV2 =  ( 3.0D0 - DSQRT(5.D0) ) / 2.D0
      
      EXPMAJ(1) = EV1 - 1.D0
      EXPMAJ(2) = 1.D0
      EXPMAJ(3) = 0.D0
      
      CALL VHATIP ( EXPMAJ )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL VSCLIP ( 1.0D0 / DSQRT(EV2), EXPMAJ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      EXPMIN(1) =  -1.D0
      EXPMIN(2) =  EV1 - 1.D0
      EXPMIN(3) = 0.D0
      
      CALL VHATIP ( EXPMIN )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL VSCLIP ( 1.0D0 / DSQRT(EV1),  EXPMIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      
      CALL SAELGV ( VEC1, VEC2, SMAJOR, SMINOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEP = VSEP ( SMAJOR, EXPMAJ )

      IF ( SEP .LT. PI() ) THEN

         CALL CHCKAD ( 'SMAJOR', SMAJOR, '~~/', EXPMAJ, 3, LIMIT, OK )

      ELSE

         CALL VSCLIP ( -1.D0, EXPMAJ )
         CALL CHCKAD ( 'SMAJOR', SMAJOR, '~~/', EXPMAJ, 3, LIMIT, OK )
         
      END IF


      SEP = VSEP ( SMINOR, EXPMIN )

      IF ( SEP .LT. PI() ) THEN

         CALL CHCKAD ( 'SMINOR', SMINOR, '~~/', EXPMIN, 3, LIMIT, OK )

      ELSE

         CALL VSCLIP ( -1.D0, EXPMIN )
         CALL CHCKAD ( 'SMINOR', SMINOR, '~~/', EXPMIN, 3, LIMIT, OK )
         
      END IF



C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Exceptional case: one zero-length generating vector.'
      CALL TCASE  ( TITLE )

      VEC1(1) =  0.D0
      VEC1(2) =  0.D0
      VEC1(3) =  0.D0
      
      VEC2(1) =  1.D0
      VEC2(2) =  2.D0
      VEC2(3) =  3.D0

      CALL SAELGV ( VEC1, VEC2, SMAJOR, SMINOR )

      CALL CHCKXC ( .FALSE., ' ', OK )
         
      CALL VEQU ( VEC1, EXPMIN )
      CALL VEQU ( VEC2, EXPMAJ )

      CALL CHCKAD ( 'SMAJOR', SMAJOR, '~~/', EXPMAJ, 3, LIMIT, OK )
      CALL CHCKAD ( 'SMINOR', SMINOR, '~~/', EXPMIN, 3, LIMIT, OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Exceptional case: two zero-length generating vectors.'
      CALL TCASE  ( TITLE )

      VEC1(1) =  0.D0
      VEC1(2) =  0.D0
      VEC1(3) =  0.D0
      
      CALL VEQU ( VEC1, VEC2 )

      CALL SAELGV ( VEC1, VEC2, SMAJOR, SMINOR )

      CALL CHCKXC ( .FALSE., ' ', OK )
         
      CALL VEQU ( VEC1, EXPMIN )
      CALL VEQU ( VEC2, EXPMAJ )

      CALL CHCKAD ( 'SMAJOR', SMAJOR, '~~/', EXPMAJ, 3, LIMIT, OK )
      CALL CHCKAD ( 'SMINOR', SMINOR, '~~/', EXPMIN, 3, LIMIT, OK )


C
C     Now we perform the pseudo-random test cases.
C
      SEED = -1

C
C     Determine the number of general cases from the dimensions
C     of the parameter set.
C
      DO CASE = 1, NCASE
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Create two generating vectors whose components ' //
     .           'range over the unit sphere; also select a '      //
     .           'scale factor in the range 1.e-306: 1.e306; '     //
     .           'case #.'

         CALL REPMI  ( TITLE, '#', CASE, TITLE )      
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL TCASE  ( TITLE )

C
C        Make up some generating vectors.  
C         
         VEC1(1) = T_RANDD ( -1.D0, 1.D0, SEED )
         VEC1(2) = T_RANDD ( -1.D0, 1.D0, SEED )
         VEC1(3) = T_RANDD ( -1.D0, 1.D0, SEED )
      
         VEC2(1) = T_RANDD ( -1.D0, 1.D0, SEED )
         VEC2(2) = T_RANDD ( -1.D0, 1.D0, SEED )
         VEC2(3) = T_RANDD ( -1.D0, 1.D0, SEED )

C            WRITE (*,*) 'VEC1  = ', VEC1
C            WRITE (*,*) 'VEC2  = ', VEC2

         SEP = VSEP( VEC1, VEC2 )

         IF (        (  SEP             .GT. LIMIT ) 
     .        .AND.  (  ABS(PI() - SEP) .GT. LIMIT )  ) THEN

C
C           Now let's get a scale factor.
C      
            SCALE = 10.D0 ** T_RANDD ( -EMAX, EMAX, SEED )


C            WRITE (*,*) 'VEC1  = ', VEC1
C            WRITE (*,*) 'VEC2  = ', VEC2
C            WRITE (*,*) 'SCALE = ', SCALE
C
C           Scale the generating vectors.
C
            CALL VSCLIP ( SCALE, VEC1 )
            CALL VSCLIP ( SCALE, VEC2 )

C
C           The call.
C
            CALL SAELGV ( VEC1, VEC2, SMAJOR, SMINOR )

            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Make sure the semi-axes are ordered properly.
C
            CALL CHCKSD ( 'VNORM(SMAJOR)-VNORM(SMINOR)',
     .                     VNORM(SMAJOR)-VNORM(SMINOR),
     .                    '>=',     0.D0,     0.D0,         OK  )
C
C           Make an ellipse out of the center and generating vectors.  
C           This involves a call to SAELGV.
C
            CALL CGV2EL ( CENTER, VEC1, VEC2, ELLIPS )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Take a sample of points on the original ellipse, and
C           find the scaled distance of each one from ELLIPS.  The
C           scaled distances should not exceed our chosen limit.
C        
            DO J = 0, NPT - 1
         
               ANGLE = J * TWOPI() / NPT
            
               CALL VLCOM  ( DCOS(ANGLE),   VEC1,
     .                       DSIN(ANGLE),   VEC2,  POINT )
               CALL CHCKXC ( .FALSE., ' ', OK )
     
               CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST  )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSD ( 'DIST/SCALE', DIST/SCALE, 
     .                       '~',          0.D0,       LIMIT,  OK )    
            END DO

         END IF

      END DO

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


