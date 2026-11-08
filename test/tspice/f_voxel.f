C$Procedure F_VOXEL ( VOXEL utility tests )
 
      SUBROUTINE F_VOXEL ( OK )
 
C$ Abstract
C
C     Exercise private SPICELIB routine associated with voxel grids.
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
C     This test family exercises private SPICELIB routine associated
C     with voxel grids. Routines covered by this family are:
C
C        ZZGETVOX
C        ZZINGRD
C        ZZTOGRID
C        ZZVOX2ID
C        ZZVOXCVO
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
C-    TSPICE Version 1.0.0, 13-FEB-2017 (NJB)
C
C        Corrected ZZGETVOX test titles.
C
C        Previous version 18-MAY-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               ZZVOX2ID
      LOGICAL               ZZINGRD

C
C     Local Parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )
 
C
C     Local Variables
C
      DOUBLE PRECISION      GRDCOR ( 3 )
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VOXORI ( 3 )
      DOUBLE PRECISION      VOXSIZ
      DOUBLE PRECISION      XGRCOR ( 3 )

      INTEGER               CGNVOX ( 3 )
      INTEGER               CGOF1D
      INTEGER               CGOFF  ( 3 )
      INTEGER               CGRCOR ( 3 )
      INTEGER               CGRSCL
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               M
      INTEGER               NVOX   ( 3 )
      INTEGER               VOXCOR ( 3 )
      INTEGER               VOXEL  ( 3 )
      INTEGER               VOXID
      INTEGER               XCGCOR ( 3 )
      INTEGER               XCGO1D
      INTEGER               XCGOFF ( 3 )
      INTEGER               XVOXEL ( 3 )
      INTEGER               XVOXID

      LOGICAL               ISIN
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
      CALL TOPEN ( 'F_VOXEL' )


C***********************************************************************
C
C     ZZINGRD tests
C
C***********************************************************************


C***********************************************************************
C
C     ZZINGRD Normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZINGRD: Point is inside grid.' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)
               
               VOXEL(3) = K

               XIN = .TRUE.

               ISIN = ZZINGRD ( NVOX, VOXEL )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )
               
            END DO

         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Point is outside grid (all components)' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, 2

         VOXEL(1) = (I-1) * (NVOX(1)+1)

         DO J = 1, 2

            VOXEL(2) = (J-1) * (NVOX(2)+1)

            DO K = 1, 2
               
               VOXEL(3) = (K-1) * (NVOX(3)+1)

               XIN = .FALSE.

               ISIN = ZZINGRD ( NVOX, VOXEL )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )
               
            END DO

         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Point is outside grid (X only' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, 2

         VOXEL(1) = (I-1) * (NVOX(1)+1)

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)
               
               VOXEL(3) = K

               XIN = .FALSE.

               ISIN = ZZINGRD ( NVOX, VOXEL )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )
               
            END DO

         END DO

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Point is outside grid (Y only' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, 2

            VOXEL(2) = (J-1) * (NVOX(2)+1)

            DO K = 1, NVOX(3)
               
               VOXEL(3) = K

               XIN = .FALSE.

               ISIN = ZZINGRD ( NVOX, VOXEL )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )
               
            END DO

         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Point is outside grid (Z only' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, 2
               
               VOXEL(3) = (K-1) * (NVOX(3) + 1)

               XIN = .FALSE.

               ISIN = ZZINGRD ( NVOX, VOXEL )

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )
               
            END DO

         END DO

      END DO

 

C*********************************************************************
C*
C*    ZZINGRD Non-error exceptional cases
C*
C*********************************************************************
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Grid dimensions are negative' )


      NVOX(1) = -1 
      NVOX(2) = -2
      NVOX(3) = -3

      VOXEL(1) = 0
      VOXEL(2) = 0
      VOXEL(3) = 0

      XIN = .FALSE.

      ISIN = ZZINGRD ( NVOX, VOXEL )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )
     




C***********************************************************************
C
C     ZZTOGRID tests
C
C***********************************************************************

 
C***********************************************************************
C
C     ZZTOGRID normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZTOGRID normal case' )


      CALL VPACK (   10.D0, 20.D0, -30.D0, VOXORI )
      CALL VPACK ( -100.D0, 70.D0, 120.D0, P      )

      VOXSIZ = 10.D0

      CALL ZZTOGRID ( P, VOXORI, VOXSIZ, GRDCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, 3
         
         XGRCOR(I) = ( P(I) - VOXORI(I) ) / VOXSIZ

      END DO

      TOL = VTIGHT

      CALL CHCKAD ( 'GDRCOR', GRDCOR, '~~/', XGRCOR, 3, TOL, OK )

      

C*********************************************************************
C*
C*    ZZTOGRID error cases
C*
C*********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZTOGRID non-positive voxel size.' )


      CALL VPACK (   10.D0, 20.D0, -30.D0, VOXORI )
      CALL VPACK ( -100.D0, 70.D0, 120.D0, P      )

      VOXSIZ = 0.D0

      CALL ZZTOGRID ( P, VOXORI, VOXSIZ, GRDCOR )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEVALUE)', OK )

      VOXSIZ = -1.D0

      CALL ZZTOGRID ( P, VOXORI, VOXSIZ, GRDCOR )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEVALUE)', OK )



C***********************************************************************
C
C     ZZGETVOX tests
C
C***********************************************************************

 
C***********************************************************************
C
C     ZZGETVOX normal cases
C
C***********************************************************************
 
       




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGETVOX: Point is at voxel center.' )

      VOXSIZ  = 10.D0

      CALL VPACK ( 3.D1, 2.D1, -5.D1, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)
               
               VOXEL(3) = K

               DO M = 1, 3
                  
                  P(M) = VOXORI(M) + ( VOXEL(M) - 1 + 0.5D0 ) * VOXSIZ

               END DO

               CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
               CALL CHCKXC ( .FALSE., ' ', OK )

               XIN = .TRUE.
               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )
               
               CALL CHCKAI ( 'VOXCOR', VOXCOR, '=', VOXEL, 3, OK )

            END DO

         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGETVOX: Point is at high-indexed corner '
     .//           'of voxel.'                                )

C
C     Use integer values throughout to avoid round-off.
C
      VOXSIZ  = 10.D0

      CALL VPACK ( 3.D1, 2.D1, -5.D1, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)
               
               VOXEL(3) = K

               DO M = 1, 3
                  
                  P(M) = VOXORI(M) + VOXEL(M) * VOXSIZ

               END DO

               CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
               CALL CHCKXC ( .FALSE., ' ', OK )

               XIN = .TRUE.
               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

               DO M = 1, 3

                  XVOXEL(M) = MIN(  VOXEL(M) + 1,  NVOX(M)  )

               END DO
C
C              The validity of this check relies on accurate integer
C              arithmetic being done with values of double precision
C              type.
C                
               CALL CHCKAI ( 'VOXCOR', VOXCOR, '=', XVOXEL, 3, OK )
               
            END DO

         END DO

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGETVOX: Point is at low-indexed corner '
     .//           'of voxel.'                                )
C
C     Use integer values throughout to avoid round-off.
C

      VOXSIZ  = 10.D0

      CALL VPACK ( 3.D1, 2.D1, -5.D1, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)
               
               VOXEL(3) = K

               DO M = 1, 3
                  
                  P(M) = VOXORI(M) + ( VOXEL(M) - 1 ) * VOXSIZ

               END DO

               CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
               CALL CHCKXC ( .FALSE., ' ', OK )

               XIN = .TRUE.
               CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

               DO M = 1, 3
                  XVOXEL(M) = VOXEL(M)                 
               END DO
C
C              The validity of this check relies on accurate integer
C              arithmetic being done with values of double precision
C              type.
C                                
               CALL CHCKAI ( 'VOXCOR', VOXCOR, '=', XVOXEL, 3, OK )
               
            END DO

         END DO

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; X < min X' )
 

      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) - 1.D-13
      P(2) = VOXORI(2) + 1.D0
      P(3) = VOXORI(3) + 1.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; X > max X' )
 

      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) + NVOX(1)*VOXSIZ + 1.D-13
      P(2) = VOXORI(2) + 1.D0
      P(3) = VOXORI(3) + 1.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; Y < min Y' )
 

      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) + 1.D0
      P(2) = VOXORI(2) - 1.D-13
      P(3) = VOXORI(3) + 1.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; Y > max Y' )
 

      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) + 1.D0
      P(2) = VOXORI(2) + VOXSIZ*NVOX(2) + 1.D-13
      P(3) = VOXORI(3) + 1.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; Z < min Z' )
 

      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) + 1.D0
      P(2) = VOXORI(2) + 1.D0
      P(3) = VOXORI(3) - 1.D-13

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGETVOX: Point is outside grid; Z > max Z' )
 

      VOXSIZ  = 20.D0

      CALL VPACK ( 3.D2, 2.D2, -5.D2, VOXORI )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      P(1) = VOXORI(1) + 1.D0
      P(2) = VOXORI(2) + 1.D0
      P(3) = VOXORI(3) + VOXSIZ*NVOX(3) +  1.D-13

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.
      CALL CHCKSL ( 'ISIN', ISIN, XIN, OK )




C***********************************************************************
C
C     ZZGETVOX error cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZGETVOX non-positive voxel size.' )



      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(3) = 7

      CALL VPACK (   10.D0, 20.D0, -30.D0, VOXORI )
      CALL VPACK ( -100.D0, 70.D0, 120.D0, P      )

      VOXSIZ = 0.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEVALUE)', OK )

      VOXSIZ = -1.D0

      CALL ZZGETVOX ( VOXSIZ, VOXORI, NVOX, P, ISIN, VOXCOR )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEVALUE)', OK )


C***********************************************************************
C
C     ZZVOX2ID tests
C
C***********************************************************************

 
C***********************************************************************
C
C     ZZVOX2ID normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZVOX2ID: point is inside grid.' )


      NVOX(1) = 10
      NVOX(2) = 15
      NVOX(2) = 7

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)
               
               VOXEL(3) = K

 
               VOXID = ZZVOX2ID ( VOXEL, NVOX )

               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Compute the 1-D index of the voxel. The index
C              is 1-based.
C
               XVOXID =     (VOXEL(3)-1)*NVOX(2)*NVOX(1)
     .                   +  (VOXEL(2)-1)*NVOX(1)
     .                   +   VOXEL(1)

               CALL CHCKSI ( 'VOXID', VOXID, '=', XVOXID, 0, OK )

            END DO

         END DO

      END DO       

C***********************************************************************
C
C     ZZVOX2ID error cases
C
C***********************************************************************

C
C     None. The routine has no special logical to deal with voxel 
C     coordinates that are out of range.
C

C***********************************************************************
C
C     ZZVOXCVO tests
C
C***********************************************************************

 
C***********************************************************************
C
C     ZZVOXCVO normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZVOXCVO: point is inside grid.' )


      CGRSCL  = 3

      CALL FILLI ( CGRSCL, 3, CGNVOX )


      NVOX(1) = 10 * CGRSCL
      NVOX(2) = 15 * CGRSCL
      NVOX(2) = 7  * CGRSCL

      DO I = 1, NVOX(1)

         VOXEL(1) = I

         DO J = 1, NVOX(2)

            VOXEL(2) = J

            DO K = 1, NVOX(3)
               
               VOXEL(3) = K


               CALL ZZVOXCVO ( VOXEL,  NVOX,  CGRSCL, 
     .                         CGRCOR, CGOFF, CGOF1D  )
               CALL CHCKXC ( .FALSE., ' ', OK )
               
C
C              Compute the expected coordinates of the coarse voxel
C              containing the fine voxel VOXEL. The coordinates
C              are 1-based.
C
               DO M = 1, 3
                  XCGCOR(M) = ( ( VOXEL(M) - 1 ) / CGRSCL ) + 1
               END DO

               CALL CHCKAI ( 'CGRCOR', CGRCOR, '=', XCGCOR, 3, OK )

C
C              Compute the expected coordinates of the fine voxel
C              relative to the base of the coarse voxel containing it.
C              The relative coordinates are 1-based.
C
               DO M = 1, 3
                  XCGOFF(M) = VOXEL(M) - ( (CGRCOR(M)-1) * CGRSCL )
               END DO

               CALL CHCKAI ( 'XCGOFF', XCGOFF, '=', CGOFF, 3, OK )

C
C              Convert the expected fine voxel offset to a 1-D offset.
C
               XCGO1D = ZZVOX2ID ( CGOFF, CGNVOX )

               CALL CHCKSI ( 'CGOF1D', CGOF1D, '=', XCGO1D, 0, OK )

            END DO

         END DO

      END DO




C***********************************************************************
C
C     ZZVOXCVO error cases
C
C***********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZVOXCVO: non-positive fine grid dimensions.' )

      CGRSCL  = 2

      NVOX(1) = -1   * CGRSCL
      NVOX(2) =  15  * CGRSCL
      NVOX(3) =  7   * CGRSCL
      
      CALL FILLI ( 1, 3, VOXEL )

      CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      NVOX(1) =  1   * CGRSCL
      NVOX(2) = -15  * CGRSCL
      NVOX(3) =  7   * CGRSCL
 
      CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      NVOX(1) =   1  * CGRSCL
      NVOX(2) =  15  * CGRSCL
      NVOX(3) =  -7  * CGRSCL
 
      CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZVOXCVO: fine voxel coordinates out of range.' )

      CGRSCL  = 2

      NVOX(1) =   1  * CGRSCL
      NVOX(2) =  15  * CGRSCL
      NVOX(3) =   7  * CGRSCL


      DO I = 1, 3

         CALL FILLI ( 1, 3, VOXEL )
         
         VOXEL(I) = 0

         CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

         VOXEL(I) = NVOX(I) + 1

         CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
         CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      END DO

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZVOXCVO: non-positive coarse voxel scale.' )

      CGRSCL  = 0

      NVOX(1) =   1  * CGRSCL
      NVOX(2) =  15  * CGRSCL
      NVOX(3) =   7  * CGRSCL


      CALL FILLI ( 1, 3, VOXEL )
         
      CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CGRSCL = -1

      CALL ZZVOXCVO ( VOXEL, NVOX, CGRSCL, CGRCOR, CGOFF, CGOF1D  )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C ---------------------------------------------------------
C


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


