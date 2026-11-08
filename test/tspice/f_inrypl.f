C$Procedure F_INRYPL ( INRYPL tests )
 
      SUBROUTINE F_INRYPL ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB ray-plane intersection routine
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
C     This routine tests the SPICELIB routine INRYPL.
C
C     Overview of test procedure
C     --------------------------
C     
C     There are three general kinds of tests applied to whack the 
C     candidate SPICELIB subroutine around.  The categories are:
C      
C        -- Normal (non-exceptional) test cases whose results are
C           easy to check by hand.  These allow a tester to verify
C           that the tested routine is at least doing reasonable-looking
C           things.  (Expected results are shown for comparison.)
C           
C        -- Normal test cases based on random inputs.  These
C           allow testing of a large number of cases.  Results will
C           be checked as follows:
C           
C                The distance between the ray's vertex VERTEX and
C                the intercept point XPT will be computed.  This
C                distance will be used to scale the negative of the
C                ray's unit direction vector.  The scaled vector
C                will be added to XPT.  The difference between the
C                resulting vector and VERTEX will be divided by
C                the distance between VERTEX and XPT, and this scaled
C                difference will be used a measure of the error made
C                in computing the intercept point.
C                
C        
C        -- Exceptional cases.  All of the error detection code will
C           be exercised by these cases.
C
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
C-    TSPICE Version 3.0.0, 01-MAY-2012 (NJB)
C
C        Loosened relative error tolerance for random
C        cases to 1e-1l.
C     
C-    TSPICE Version 2.0.0, 23-OCT-2006 (NJB)
C      
C        Corrected sign of expected intercept Z-component for 
C        "normal test" 10.  Also corrected typo in the associated
C        comment.
C
C-    TSPICE Version 1.0.0, 20-OCT-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP


C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD

C
C     Local Parameters
C
      DOUBLE PRECISION      EBIG
      PARAMETER           ( EBIG   = 1.D306 )

      DOUBLE PRECISION      EBIGN
      PARAMETER           ( EBIGN  = -EBIG )

      DOUBLE PRECISION      EBIG2
      PARAMETER           ( EBIG2  = 2.D306 )

      DOUBLE PRECISION      ESMALL
      PARAMETER           ( ESMALL = 1.D-306 )

      DOUBLE PRECISION      ESMALN
      PARAMETER           ( ESMALN = -1.D-306 )

      DOUBLE PRECISION      ESMAL2
      PARAMETER           ( ESMAL2 = 2.D-306 )


      INTEGER               EMAX
      PARAMETER           ( EMAX   = 306 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NBASIC
      PARAMETER           ( NBASIC =  11 )
    
      INTEGER               NRAND
      PARAMETER           ( NRAND  = 5000 )
    
      INTEGER               NCASE
      PARAMETER           ( NCASE  = NBASIC + NRAND )
      
      INTEGER               LINLEN
      PARAMETER           ( LINLEN =  79 )
      
      INTEGER               UBPL
      PARAMETER           ( UBPL   =   4 )
      
      DOUBLE PRECISION      MAXRE
      PARAMETER           ( MAXRE  =  1.D-11 )

C
C     Local Variables
C      
      CHARACTER*(MSGLEN)    TITLE
      
            
      DOUBLE PRECISION      C        
      DOUBLE PRECISION      CONS
      DOUBLE PRECISION      D      ( 3 )
      DOUBLE PRECISION      DIFF   ( 3 )
      DOUBLE PRECISION      DIR    ( 3, NBASIC )
      DOUBLE PRECISION      ERRC   
      DOUBLE PRECISION      ERRD   ( 3 )
      DOUBLE PRECISION      ERRN   ( 3 )
      DOUBLE PRECISION      ERROR  ( 3 )
      DOUBLE PRECISION      ERRV   ( 3 )
      DOUBLE PRECISION      EXPXPT ( 3, NBASIC )
      DOUBLE PRECISION      N      ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PLANE  ( UBPL )      
      DOUBLE PRECISION      PLCONS (    NBASIC )
      DOUBLE PRECISION      PLNORM ( 3, NBASIC )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      TOOBIG
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      VERTEX ( 3, NBASIC )
      DOUBLE PRECISION      VPRJ   ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               EXPNO  (    NBASIC )
      INTEGER               NXPTS
      INTEGER               I
      INTEGER               SEED
           
      LOGICAL               SEPOK

C
C     Saved variables
C
      SAVE                  PLCONS
      SAVE                  PLNORM
      SAVE                  VERTEX
      SAVE                  DIR
      SAVE                  EXPNO
      SAVE                  EXPXPT
      
C
C     Initial values
C


C
C     Normal test cases:
C


C     
C     1)  
C     
C     Planes' normal vector:                  ( 0,   0,   1 )
C     Plane constant:                           0
C     Ray's vertex:                           ( 0,   0,   2 )
C     Ray's direction vector:                 ( 0,   0,  -1 )
C     Expected number of intercept points:      1
C     Expected intercept point:               ( 0,   0,   0 )
C     
C         
      DATA    PLCONS(1),             ( PLNORM (I,1), I = 1, 3  )       /
     .    
     .        0.D0,                     0.D0,  0.D0,   1.D0            /
         
         
      DATA  ( VERTEX(I,1), I=1,3 ),  ( DIR(I,1),     I = 1, 3  )       /
     .    
     .        0.D0,  0.D0,  2.D0,       0.D0,  0.D0,  -1.D0            /
         
          
      DATA   EXPNO(1),               ( EXPXPT(I,1),  I = 1, 3  )       /
     .    
     .        1,                        0.D0,   0.D0,  0.D0            /



C     
C     2)  
C     
C     Planes' normal vector:                  ( 0,     0,     1     )
C     Plane constant:                           10
C     Ray's vertex:                           ( 0,     0,     20    )
C     Ray's direction vector:                 ( 0,     3,    -1     )
C     Expected number of intercept points:      1
C     Expected intercept point:               ( 0,     30     10    )
C     
C         
      DATA    PLCONS(2),             ( PLNORM (I,2), I = 1, 3 )        /
     .    
     .        1.D1,                    0.D0,  0.D0,   1.D0             /
         
         
      DATA  ( VERTEX(I,2), I=1,3 ),  ( DIR    (I,2), I = 1, 3 )        /
     .    
     .        0.D0,  0.D0,  2.D1,      0.D0,  3.D0,  -1.D0             /
          
          
      DATA   EXPNO(2),               ( EXPXPT (I,2),  I = 1,3 )        /
     .    
     .        1,                        0.D0,  3.D1,  1.D1             /



C     
C     3)  
C     
C     Planes' normal vector:                  ( 0,   0,   1 )
C     Plane constant:                           0
C     Ray's vertex:                           ( 1,   1,   0 )
C     Ray's direction vector:                 ( 0,   0,  -1 )
C     Expected number of intercept points:      1
C     Expected intercept point:               ( 1,   1,   0 )
C         
C         
      DATA    PLCONS(3),             ( PLNORM (I,3), I = 1, 3  )       /
     .    
     .        0.D0,                     0.D0,  0.D0,   1.D0            /
         
         
      DATA  ( VERTEX(I,3), I=1,3 ),  ( DIR    (I,3), I = 1, 3  )       /
     .    
     .        0.D0,  0.D0,  0.D0,       0.D0,  0.D0,  -1.D0            /
         
          
      DATA   EXPNO(3),               ( EXPXPT (I,3), I = 1, 3  )       /
     .    
     .        1,                        0.D0,   0.D0,  0.D0            /



C     
C     4)
C     
C     Planes' normal vector:                  ( 0,   1,   0 )
C     Plane constant:                           0
C     Ray's vertex:                           ( 0,   0,   0 )
C     Ray's direction vector:                 ( 1,   0,   0 )
C     Expected number of intercept points:     -1 (infinite)
C     Expected intercept point:               ( 0,   0,   0 )
C         
C         
      DATA    PLCONS(4),             ( PLNORM (I,4), I = 1, 3  )       /
     .    
     .        0.D0,                     0.D0,  1.D0,   0.D0            /
         
         
      DATA  ( VERTEX(I,4), I=1,3 ),  ( DIR    (I,4), I = 1, 3  )       /
     .    
     .        0.D0,  0.D0,  0.D0,       1.D0,  0.D0,   1.D0            /
         
          
      DATA   EXPNO(4),               ( EXPXPT (I,4), I = 1, 3  )       /
     .    
     .       -1,                        0.D0,   0.D0,  0.D0            /




C     
C     5)    
C     
C     Planes' normal vector:                  ( 0,   0,   1 )
C     Plane constant:                           0
C     Ray's vertex:                           ( 1,   1,   1 )
C     Ray's direction vector:                 ( 1,   0,   0 )
C     Expected number of intercept points:      0
C     Expected intercept point:               ( 0,   0,   0 )
C         
C         
      DATA    PLCONS(5),             ( PLNORM (I,5), I = 1, 3  )       /
     .    
     .        0.D0,                     0.D0,  0.D0,   1.D0            /
         
         
      DATA  ( VERTEX(I,5), I=1,3 ),  ( DIR    (I,5), I = 1, 3  )       /
     .    
     .        1.D0,  1.D0,  1.D0,       1.D0,  0.D0,   0.D0            /
         
          
      DATA    EXPNO(5),              ( EXPXPT (I,5), I = 1, 3  )       /
     .    
     .        0,                        0.D0,   0.D0,  0.D0            /



C     
C     6)    
C     
C     Planes' normal vector:                  ( 0,     0,       1 )
C     Plane constant:                           0
C     Ray's vertex:                           ( 1,     1,       1 )
C     Ray's direction vector:                 ( 0,     1.D16,  -1 )
C     Expected number of intercept points:      1
C     Expected intercept point:               ( 1,     1.D16,   0 )
C         
C         
      DATA    PLCONS(6),             ( PLNORM (I,6), I = 1, 3  )       /
     .    
     .        0.D0,                     0.D0,  0.D0,   1.D0            /
         
         
      DATA  ( VERTEX(I,6), I=1,3 ),  ( DIR    (I,6), I = 1, 3  )       /
     .    
     .        1.D0,  1.D0,  1.D0,       0.D0,  1.D16,  -1.D0           /
         
          
      DATA    EXPNO(6),              ( EXPXPT (I,6), I = 1, 3  )       /
     .    
     .        1,                        1.D0,  1.D16,  0.D0            /



C     
C     7)    
C     
C     Planes' normal vector:                  ( 0,     0,       1 )
C     Plane constant:                           0
C     Ray's vertex:                           ( 1,     1,       1 )
C     Ray's direction vector:                 ( 0,     1.D308, -1 )
C     Expected number of intercept points:      0
C     Expected intercept point:               ( 0,     0,       0 )
C         
C         
      DATA    PLCONS(7),             ( PLNORM (I,7), I = 1, 3  )       /
     .    
     .        0.D0,                     0.D0,  0.D0,   1.D0            /
         
         
      DATA  ( VERTEX(I,7), I=1,3 ),  ( DIR    (I,7), I = 1, 3  )       /
     .    
     .        1.D0,  1.D0,  1.D0,       0.D0,  1.D308,  -1.D0          /
         
          
      DATA    EXPNO(7),              ( EXPXPT (I,7), I = 1, 3  )       /
     .    
     .        0,                        0.D0,  0.D0,    0.D0           /


C     
C     8)         
C     
C     Planes' normal vector:                  ( 0,     0,       1     )
C     Plane constant:                           EBIG
C     Ray's vertex:                           ( 1,     1,      2*EBIG )
C     Ray's direction vector:                 ( 0,     100,    -1     )
C     Expected number of intercept points:      0
C     Expected intercept point:               ( 0,     0,       0 )
C         
C         
      DATA    PLCONS(8),             ( PLNORM (I,8), I = 1, 3  )      /
     .    
     .        EBIG,                    0.D0,  0.D0,   1.D0            /
         
         
      DATA  ( VERTEX(I,8), I=1,3 ),  ( DIR    (I,8), I = 1, 3  )      /
     .    
     .        1.D0,  1.D0,  EBIG2,     0.D0,  1.D2,   -1.D0           /
         
          
      DATA    EXPNO(8),              ( EXPXPT (I,8), I = 1, 3  )      /
     .    
     .        0,                        0.D0,  0.D0,    0.D0          /


C     
C     9)         
C     
C     Planes' normal vector:                  ( 0,      0,       1    )
C     Plane constant:                          -1.D306 
C     Ray's vertex:                           ( 1.D306,  0,      0    )
C     Ray's direction vector:                 ( 1,      0,      -1    )
C     Expected number of intercept points:      1
C     Expected intercept point:               ( 2.D306,  0,   -1.D306 )
C         
C         
      DATA    PLCONS(9),             ( PLNORM (I,9), I = 1, 3  )       /
     .    
     .        EBIGN,                     0.D0,  0.D0,   1.D0           /
         
         
      DATA  ( VERTEX(I,9), I=1,3 ),  ( DIR    (I,9), I = 1, 3  )       /
     .    
     .        EBIG,  0.D0,  0.D0,        1.D0,  0.D0,   -1.D0          /
         
          
      DATA    EXPNO(9),              ( EXPXPT (I,9), I = 1, 3  )       /
     .    
     .        1,                         EBIG2, 0.D0,  EBIGN           /


C     
C     10)         
C     
C     Planes' normal vector:                  ( 0,       0,       1    )
C     Plane constant:                          -1.D-306 
C     Ray's vertex:                           ( 1.D-306,  0,      0    )
C     Ray's direction vector:                 ( 1,       0,      -1    )
C     Expected number of intercept points:      1
C     Expected intercept point:               ( 2.D-306,  0,  -1.D-306 )
C         
C         
      DATA    PLCONS(10),             ( PLNORM (I,10), I = 1, 3  )     /
     .    
     .       ESMALN,                    0.D0,  0.D0,   1.D0            /
         
         
      DATA  ( VERTEX(I,10), I=1,3 ),  ( DIR    (I,10), I = 1, 3  )     /
     .    
     .        ESMALL,  0.D0,  0.D0,     1.D0,  0.D0,   -1.D0           /
         
          
      DATA    EXPNO(10),              ( EXPXPT (I,10), I = 1, 3  )     /
     .    
     .        1,                        ESMAL2, 0.D0, ESMALN          /


C     
C     11)         
C     
C     Planes' normal vector:                  ( 0,       0,       1    )
C     Plane constant:                           1
C     Ray's vertex:                           ( 0     ,  0,       0    )
C     Ray's direction vector:                 ( 1,       0,      -1    )
C     Expected number of intercept points:      0
C     Expected intercept point:               ( 0,       0,       0    )
C         
C         
      DATA    PLCONS(11),             ( PLNORM (I,11), I = 1, 3  )     /
     .    
     .        1,                        0.D0,  0.D0,   1.D0            /
         
         
      DATA  ( VERTEX(I,11), I=1,3 ),  ( DIR    (I,11), I = 1, 3  )     /
     .    
     .        0.D0,  0.D0,  0.D0,       1.D0,  0.D0,   -1.D0           /
         
          
      DATA    EXPNO(11),              ( EXPXPT (I,11), I = 1, 3  )     /
     .    
     .        0,                        0.D0, 0.D0, 0.D0               /
     



C
C     Open the test family.
C
      CALL TOPEN ( 'F_INRYPL' )

C
C     We'll start out with some easy-to-check cases (11 of them).
C
      DO I = 1, NBASIC
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Basic case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )

C
C        Make a SPICELIB plane from the plane constant and normal
C        vector.
C
         CALL NVC2PL ( PLNORM(1,I), PLCONS(I), PLANE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PL2NVC ( PLANE, NORMAL, CONS )
         CALL CHCKXC ( .FALSE., ' ', OK )      
C      
C        Call the routine to be tested.
C    
         CALL INRYPL ( VERTEX(1,I), DIR(1,I), PLANE, NXPTS, XPT )
         CALL CHCKXC ( .FALSE., ' ', OK )      


         CALL CHCKSI (  'NXPTS---number of intercept points',
     .                  NXPTS,
     .                  '=',
     .                  EXPNO(I),
     .                  0,
     .                  OK                                   )

         CALL CHCKAD ( 'XPT---intercept', XPT,  '~~/', 
     .                  EXPXPT(1,I),      3,    MAXRE,  OK )

      END DO



C
C     Random cases follow.
C
      SEED = -1

      DO I = 1, NRAND
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Random case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE  ( TITLE )
C
C        Generate a random scale factor.
C
         SCALE = 10.D0 ** T_RANDD ( -DBLE(EMAX), DBLE(EMAX), SEED )
         
C
C        Generate a normal vector and plane constant, and from these
C        a SPICELIB plane.
C
         N(1)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         N(2)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         N(3)  =  T_RANDD ( -2.D0, 2.D0, SEED )

         CALL VHATIP ( N )
         CALL CHCKXC ( .FALSE., ' ', OK )

         C     =  T_RANDD ( -2.D0, 2.D0, SEED )  *  SCALE

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL NVC2PL ( N, C, PLANE )
         
C
C        Now generate a random ray vertex and ray direction vector.
C         
         V(1)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         V(2)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         V(3)  =  T_RANDD ( -2.D0, 2.D0, SEED )

         CALL VSCLIP ( SCALE, V )
         CALL CHCKXC ( .FALSE., ' ', OK )

         D(1)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         D(2)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         D(3)  =  T_RANDD ( -2.D0, 2.D0, SEED )
         
C
C        The call.
C
         CALL INRYPL ( V, D, PLANE, NXPTS, XPT )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
C
C        We can pretty safely assume that we won't see a value of
C        -1 for NXPTS.  If the value is 1, we'll try to get back from 
C        XPT to the ray's vertex.
C
         IF ( NXPTS .EQ. 1 ) THEN
         
            CALL VSUB   (  V,  XPT,  DIFF  )
            
            CALL VHATIP ( D )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VLCOM (  1.D0,        XPT,
     .                   -VNORM(DIFF), D,    V2 )
         
            CALL CHCKAD ( 'V2', V2, '~~/', V, 3, MAXRE, OK )

            CALL VSUB  (  V,  V2,  ERROR )
             
         ELSE 
C
C           Check the angular separation between the ray
C           and the vector from the ray's vertex to its orthogonal
C           projection to the plane.
C         
            CALL VPRJP  ( V,      PLANE, VPRJ )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL VSUB   ( VPRJ,   V,     DIFF )

            SEP    =  VSEP ( DIFF,   D )
                        
            TOOBIG = DPMAX() / 3
            
            IF ( SEP*DPR() .GE. 90.D0 )THEN
C
C              The ray is parallel to or points away from the plane.
C            
               SEPOK = .TRUE.
               
            ELSE IF ( SEP .GT. DATAN2( TOOBIG, VNORM(DIFF) )  ) THEN
C
C              It doesn't happen often, but we might have a case
C              where the ray is too close to being parallel with
C              the plane for an intersection to occur.
C
               SEPOK = .TRUE.
               
            ELSE
C
C              This shouldn't happen.
C
               SEPOK = .FALSE.
               
            END IF
            
            CALL CHCKSL ( 'SEPOK---is angular separation of ray '  //
     .                    'and plane consistent with return code?',
     .                    SEPOK,
     .                    .TRUE.,
     .                    OK                                         )

         END IF
                  
      END DO


C
C     Now for the exceptions.
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exception:  ray''s direction is the zero vector.' )
      
      CALL CLEARD ( 3, ERRV )
      CALL CLEARD ( 3, ERRD )
      CALL CLEARD ( 3, ERRN )

      ERRN(3) = 1.D0
      ERRC    = 0.D0

      CALL NVC2PL ( ERRN, ERRC, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INRYPL ( ERRV, ERRD, PLANE, NXPTS, XPT )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exception:  ray''s vertex is just too big.' )
 
      CALL CLEARD ( 3, ERRV )
      CALL CLEARD ( 3, ERRD )
      CALL CLEARD ( 3, ERRN )

      ERRV(3) =  1.D308
      ERRD(3) = -1.D0
      ERRN(3) =  1.D0
      ERRC    =  0.D0

      CALL NVC2PL ( ERRN, ERRC, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INRYPL ( ERRV, ERRD, PLANE, NXPTS, XPT )
      CALL CHCKXC ( .TRUE., 'SPICE(VECTORTOOBIG)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exception:  Plane is too far from the origin.' )
 
      CALL CLEARD ( 3, ERRV )
      CALL CLEARD ( 3, ERRD )
      CALL CLEARD ( 3, ERRN )

      ERRV(3) =  1.D0
      ERRD(3) = -1.D0
      ERRN(3) =  1.D0
      ERRC    =  1.D308

      CALL NVC2PL ( ERRN, ERRC, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INRYPL ( ERRV, ERRD, PLANE, NXPTS, XPT )
      CALL CHCKXC ( .TRUE., 'SPICE(VECTORTOOBIG)', OK )


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END
