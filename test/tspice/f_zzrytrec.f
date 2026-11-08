C$Procedure F_ZZRYTREC ( ZZRYTREC tests )
 
      SUBROUTINE F_ZZRYTREC ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB routine ZZRYTREC.
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

      INCLUDE 'dskdsc.inc'
      INCLUDE 'dsktol.inc'

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
C     This routine tests the private SPICELIB routine ZZRYTREC.
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
C-    TSPICE Version 1.0.0, 22-JUL-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 

C
C     Local Parameters
C

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

C
C     Local Variables 
C
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      D      ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      H
      DOUBLE PRECISION      L
      DOUBLE PRECISION      MARGIN      
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      W
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )
      
      INTEGER               C1
      INTEGER               C2
      INTEGER               F
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               M
      INTEGER               NEXT   ( 3 )
      INTEGER               NXPTS
      INTEGER               P
      INTEGER               PERPCO ( 2 )
      INTEGER               S
      INTEGER               XNXPTS
      
           
C
C     Saved values
C
      SAVE

C
C     Initial values
C
      DATA                  NEXT / 2, 3, 1 /


C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZRYTREC' )


C
C     We rely on ZZRAYBOX to work correctly; these tests simply 
C     validate the use by ZZRYTREC of ZZRAYBOX.
C

C***********************************************************************
C
C     Normal cases
C
C***********************************************************************

 
C***********************************************************************
C
C
C     Simple hit/miss cases
C
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Hit case. Zero MARGIN.' )



      MARGIN = 0.D0 
C
C
C     Set the coordinate system and bounds.

      L =  40.D0
      W =  20.D0
      H =  10.D0

      BOUNDS(1,1) = -L/2
      BOUNDS(2,1) =  L/2
      BOUNDS(1,2) = -W/2
      BOUNDS(2,2) =  W/2
      BOUNDS(1,3) = -H/2
      BOUNDS(2,3) =  H/2

      D(1) = L
      D(2) = W
      D(3) = H

C
C     Try an intercept using rays emanating from points on
C     each side of the volume element.
C
      
      DO I = 1, 3

         DO J = 1, 2

            M = 2*J - 3

            CALL CLEARD ( 3, VERTEX )
            
            VERTEX(I) = 2 * M * D(I)

            CALL VMINUS( VERTEX, RAYDIR )



      
            CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS,
     .                      MARGIN, NXPTS,  XPT    )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           We expect an intercept.
C      
            XNXPTS = 1

            CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

C
C           Create the expected intercept point.
C
            CALL CLEARD ( 3, XXPT )
            XXPT(I) = M * D(I)/2

      
            TOL = VTIGHT

            CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

         END DO

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Miss case. Zero MARGIN.' )
 

      MARGIN = 0.D0
C
C     Try an intercept using rays emanating from points on
C     each side of the volume element. For each face, try
C     rays that miss on all sides.
C
      DELTA = 1.D-12

      DO I = 1, 3

         PERPCO(1) = NEXT(I)
         PERPCO(2) = NEXT(NEXT(I))

         DO J = 1, 2

            F = 2*J - 3

C
C           Loop over the coordinates orthogonal to coordinate I.
C
            DO K = 1, 2
               
               C1 = PERPCO(K  )
               C2 = PERPCO(3-K)

               DO P = 1, 2
C
C                 S is the sign of the displacement in the direction of
C                 C1.
C
                  S = 2*P - 3


                  CALL CLEARD ( 3, VERTEX )
            
                  VERTEX(I)  = 2 * F * D(I)
                  VERTEX(C1) = S * (  ( D(C1)/2 ) + DELTA  )
                  VERTEX(C2) = 0.D0
                  
                  CALL CLEARD( 3, RAYDIR )
                  
                  RAYDIR(I) = -F

                  MARGIN = 0.D0

                  CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS,
     .                            MARGIN, NXPTS,  XPT    )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 We expect to find no intercept.
C      
                  XNXPTS = 0

                  CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

               END DO
C
C              End of perpendicular coordinate face loop.
C
            END DO
C
C           End of perpendicular coordinate loop.
C                      
         END DO
C
C        End of primary coordinate face loop.
C
      END DO
C
C     End of primary coordinate loop.
C


 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Hit case. Positive MARGIN.' )
 


      MARGIN = 1.D-11
C
C     Try an intercept using rays emanating from points on
C     each side of the volume element. For each face, try
C     rays that hits slightly beyond the edges of the 
C     unexpanded face, but within the edges of the expanded
C     face, on all sides. 
C

      DO I = 1, 3

         PERPCO(1) = NEXT(I)
         PERPCO(2) = NEXT(NEXT(I))


         DO J = 1, 2

            F = 2*J - 3

C
C           Loop over the coordinates orthogonal to coordinate I.
C
            DO K = 1, 2
               
               C1    = PERPCO(K  )
               C2    = PERPCO(3-K)

               DELTA = ( MARGIN / 2 ) * D(C1)

               DO P = 1, 2
C
C                 S is the sign of the displacement in the direction of
C                 C1.
C
                  S = 2*P - 3


                  CALL CLEARD ( 3, VERTEX )
            
                  VERTEX(I)  = 2 * F * D(I)
                  VERTEX(C1) = S * (  ( D(C1)/2 ) + DELTA  )
                  VERTEX(C2) = 0.D0
                  
                  CALL CLEARD( 3, RAYDIR )
                  
                  RAYDIR(I) = -F


                  CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS,
     .                            MARGIN, NXPTS,  XPT    )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 We expect to find an intercept.
C      
                  XNXPTS = 1

                  CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

C
C                 Create the expected intercept point.
C
                  CALL CLEARD ( 3, XXPT )

                  XXPT(I ) = F * ( (D(I)/2) + (MARGIN*D(I)) )
                  XXPT(C1) = VERTEX(C1)
                  XXPT(C2) = VERTEX(C2)
      
                  TOL = VTIGHT

                  CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

               END DO
C
C              End of perpendicular coordinate face loop.
C
            END DO
C
C           End of perpendicular coordinate loop.
C                      
         END DO
C
C        End of primary coordinate face loop.
C
      END DO
C
C     End of primary coordinate loop.
C
 


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Miss case. Positive MARGIN.' )
 


      MARGIN = 1.D-11
C
C     Try an intercept using rays emanating from points on
C     each side of the volume element. For each face, try
C     rays that hits slightly beyond the edges of the 
C     expanded face on all sides.
C

      DO I = 1, 3

         PERPCO(1) = NEXT(I)
         PERPCO(2) = NEXT(NEXT(I))


         DO J = 1, 2

            F = 2*J - 3

C
C           Loop over the coordinates orthogonal to coordinate I.
C
            DO K = 1, 2
               
               C1    = PERPCO(K  )
               C2    = PERPCO(3-K)

               DELTA = ( MARGIN * 2 ) * D(C1)

               DO P = 1, 2
C
C                 S is the sign of the displacement in the direction of
C                 C1.
C
                  S = 2*P - 3


                  CALL CLEARD ( 3, VERTEX )
            
                  VERTEX(I)  = 2 * F * D(I)
                  VERTEX(C1) = S * (  ( D(C1)/2 ) + DELTA  )
                  VERTEX(C2) = 0.D0
                  
                  CALL CLEARD( 3, RAYDIR )
                  
                  RAYDIR(I) = -F


                  CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS,
     .                            MARGIN, NXPTS,  XPT    )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 We expect to find no intercept.
C      
                  XNXPTS = 0

                  CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )
 

               END DO
C
C              End of perpendicular coordinate face loop.
C
            END DO
C
C           End of perpendicular coordinate loop.
C                      
         END DO
C
C        End of primary coordinate face loop.
C
      END DO
C
C     End of primary coordinate loop.
C
 

C***********************************************************************
C
C     Non-error exceptional cases
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Vertex inside element. Positive MARGIN.' )
 


      MARGIN = 1.D-11
C
C     Try an intercept using rays emanating from points on
C     each side of the volume element. For each face, try
C     rays having vertices slightly beyond unexpanded element
C     but within the expanded element.
C

      DO I = 1, 3

         PERPCO(1) = NEXT(I)
         PERPCO(2) = NEXT(NEXT(I))


         DO J = 1, 2

            F = 2*J - 3

C
C           Loop over the coordinates orthogonal to coordinate I.
C
            DO K = 1, 2
               
               C1    = PERPCO(K  )
               C2    = PERPCO(3-K)

C
C              The margin used by ZZRYTREC for this test is twice the
C              input margin. Use a large delta to verify this.
C
               DELTA = ( MARGIN * 1.999D0 ) * D(C1) 

               DO P = 1, 2
C
C                 S is the sign of the displacement in the direction of
C                 C1.
C
                  S = 2*P - 3


                  CALL CLEARD ( 3, VERTEX )
            
                  VERTEX(I)  = ( D(I)/2 ) * DELTA
                  VERTEX(C1) = S * (  ( D(C1)/2 ) + DELTA  )
                  VERTEX(C2) = 0.D0
                  
                  CALL CLEARD( 3, RAYDIR )
                  
                  RAYDIR(I) = -F


                  CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS,
     .                            MARGIN, NXPTS,  XPT    )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 We expect to find an intercept.
C      
                  XNXPTS = 1

                  CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

C
C                 The expected intercept point is the vertex itself.
C
                  CALL VEQU ( VERTEX, XXPT )
            
                  CALL CHCKAD ( 'XPT', XPT, '=', XXPT, 3, 0.D0, OK )

               END DO
C
C              End of perpendicular coordinate face loop.
C
            END DO
C
C           End of perpendicular coordinate loop.
C                      
         END DO
C
C        End of primary coordinate face loop.
C
      END DO
C
C     End of primary coordinate loop.
C
 

C***********************************************************************
C
C     Error cases
C
C***********************************************************************
  
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bounds out of order.' )

      MARGIN = 0.D0


      BOUNDS(1,1) = -L/2
      BOUNDS(2,1) =  L/2
      BOUNDS(1,2) = -W/2
      BOUNDS(2,2) =  W/2
      BOUNDS(1,3) = -H/2
      BOUNDS(2,3) =  H/2

      DO I = 1, 3

         CALL SWAPD ( BOUNDS(1,I), BOUNDS(2,I) )

         CALL CLEARD( 3, VERTEX )
         VERTEX(I) = 2 * D(I)

         CALL VMINUS( VERTEX, RAYDIR )


         CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS,
     .                   MARGIN, NXPTS,  XPT    )
         CALL CHCKXC ( .TRUE., 'SPICE(BADCOORDBOUNDS)', OK )
C
C        Restore bounds.
C
         CALL SWAPD ( BOUNDS(1,I), BOUNDS(2,I) )

      END DO
 



C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


