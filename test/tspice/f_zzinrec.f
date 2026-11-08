C$Procedure F_ZZINREC ( ZZINREC tests )
 
      SUBROUTINE F_ZZINREC ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB routine ZZINREC.
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
C     This routine tests the private SPICELIB routine ZZINREC.
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
C-    TSPICE Version 1.0.0, 04-JUN-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 

C
C     Local Parameters
C

C
C     Local Variables 
C
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      L
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      MIDPT  ( 3 )
      DOUBLE PRECISION      P      ( 3 )

      INTEGER               EXCLUD      
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               M

      LOGICAL               INSIDE
      LOGICAL               XIN
           
C
C     Saved values
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZINREC' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Interior case. Exclude none. Zero MARGIN.' )
 

      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      
      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Interior case. Exclude none. Zero MARGIN. '
     .//           'Test points are near corners.'            )
 

      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      DELTA = 1.D-13

      DO I = 1, 2
C
C        Set the X value slightly above the minimum for I = 1;
C        slightly below the maximum for I = 2.
C
         M    = 3 - 2*I 

         P(1) = BOUNDS(I,1) + M*DELTA

         DO J = 1, 2

            M    = 3 - 2*J 
 
            P(2) = BOUNDS(J,2) + M*DELTA


            DO K = 1, 2

               M    = 3 - 2*K 

               P(3) = BOUNDS(K,3) + M*DELTA

      
               CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               XIN = .TRUE.

               CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

            END DO

         END DO

      END DO

      
      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Interior case. Exclude none. '
     .//           'MARGIN > 0.' )
 

 
      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0
      

      DO I = 1, 2

         L     = BOUNDS(2,1) - BOUNDS(1,1)
         DELTA = MARGIN * L / 2
C
C        Set the X value slightly below the minimum for I = 1;
C        slightly above the maximum for I = 2. The offset
C        magnitude is within that specified by the margin.
C
         M     = 3 - 2*I 
         P(1)  = BOUNDS(I,1) + M*DELTA


         DO J = 1, 2

            L     = BOUNDS(2,2) - BOUNDS(1,2)
            DELTA = MARGIN * L / 2

            M     = 3 - 2*J 
            P(2)  = BOUNDS(I,2) + M*DELTA


            DO K = 1, 2

               M    = 3 - 2*K 
               P(3) = BOUNDS(K,3) + M*DELTA

      
               CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               XIN = .TRUE.

               CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

            END DO

         END DO

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Exterior case. Exclude none. '
     .//           'MARGIN > 0.' )
 
 
      EXCLUD = 0
      MARGIN = 1.D-2


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      DO I = 1, 3

         MIDPT(I) = ( BOUNDS(1,I) + BOUNDS(2,I) ) / 2

      END DO


      DO I = 1, 3
C
C        Set the point slightly outside the bounds for
C        the Ith coordinate; the other coordinates are
C        in bounds.
C
         CALL VEQU ( MIDPT, P )


         L     = BOUNDS(2,I) - BOUNDS(1,I)
         DELTA = MARGIN * L * 2

         DO J = 1, 2

            M     = 2*J - 3
            P(I)  = BOUNDS(J,I) + M*DELTA


            CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XIN = .FALSE.

            CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

         END DO
  
      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RECSYS: interior case. Exclude a coordinate; '
     .//           'the excluded coordinate is out of range. '
     .//           'MARGIN > 0.'                                )
 
 
      MARGIN = 1.D-2

      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0
 

      DO I = 1, 3

         MIDPT(I) = ( BOUNDS(1,I) + BOUNDS(2,I) ) / 2

      END DO


      DO I = 1, 3
C
C        Set the point slightly outside the bounds for
C        the Ith coordinate; the other coordinates are
C        in bounds.
C
         CALL VEQU ( MIDPT, P )


         L     = BOUNDS(2,I) - BOUNDS(1,I)
         DELTA = MARGIN * L * 2

         DO J = 1, 2

            M      = 2*J - 3
            P(I)   = BOUNDS(J,I) + M*DELTA

            EXCLUD = I

            CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            XIN = .TRUE.

            CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

         END DO
  
      END DO


C***********************************************************************
C
C     Error cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: Negative margin.'  )


      EXCLUD = 0
      MARGIN = -1.D-12


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      
      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: Invalid exclude index.'  )


      EXCLUD = -1
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      
      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )


      EXCLUD = 4

      CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: invalid rectangular coordinate bounds.' )


      EXCLUD = 0
      MARGIN = 0.D0


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  -3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0

      
      CALL VPACK ( 2.D0, -2.D0, 7.D0, P )

      CALL ZZINREC ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )  

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


