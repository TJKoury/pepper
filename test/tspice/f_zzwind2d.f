C$Procedure      F_ZZWIND2D ( Test ZZWIND2D )
  
      SUBROUTINE F_ZZWIND2D ( OK )

C$ Abstract
C
C     Test the private 2D winding number routine ZZWIND2D.
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
C     This family tests the private 2D winding number
C     routine ZZWIND2D.
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
C-    SPICELIB Version 1.0.0, 08-JUL-2008 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
     
      INTEGER               ZZWIND2D

C     
C     Local parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Local Variables
C
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      POINT  ( 2 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      VERTCS ( 2, MAXVRT )

      INTEGER               J
      INTEGER               M
      INTEGER               N
      INTEGER               W

C
C     Save everything.
C
      SAVE
 
C
C     Initial values
C
 

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZWIND2D' )


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Number of vertices is less than 3.' )

      CALL CLEARD ( 2, POINT )
      

      N = 1
      CALL CLEARD ( 2*N, VERTCS )

      W = ZZWIND2D ( N, VERTCS, POINT )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

      N = 2
      CALL CLEARD ( 2*N, VERTCS )

      W = ZZWIND2D ( N, VERTCS, POINT )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************


C
C     Try some regular polygons; path direction is positive.
C
      DO N = 3, 10
         
C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Positively oriented, regular polygon. '
     .   //      'Number of vertices is #; POINT is origin.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         CALL CLEARD ( 2, POINT )

         DO J = 1, N

            THETA = (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA)
            VERTCS(2,J) = SIN(THETA)
            
         END DO
         
         W = ZZWIND2D ( N, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', 1, 0, OK )

      END DO



C
C     Try some regular polygons; path direction is negative.
C
      DO N = 3, 10
         
C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Negatively oriented, regular polygon. '
     .   //      'Number of vertices is #; POINT is origin.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         CALL CLEARD ( 2, POINT )

         DO J = 1, N

            THETA = - (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA)
            VERTCS(2,J) = SIN(THETA)
            
         END DO
         
         W = ZZWIND2D ( N, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', -1, 0, OK )

      END DO

C
C     Check the cases above for an exterior point.
C
      DO N = 3, 10
         
C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Negatively oriented, regular polygon. '
     .   //      'Number of vertices is #; POINT is origin.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 2.D0
         POINT(2) = 3.D0

         DO J = 1, N

            THETA = - (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA)
            VERTCS(2,J) = SIN(THETA)
            
         END DO
         
         W = ZZWIND2D ( N, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', 0, 0, OK )

      END DO


C
C     Repeat the previous cases with non-zero central point.
C
      DO N = 3, 10
         
C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Positively oriented, regular polygon. '
     .   //      'N verts is #; POINT is not origin.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 2.D0
         POINT(2) = 3.D0

         DO J = 1, N

            THETA = (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA) + POINT(1)
            VERTCS(2,J) = SIN(THETA) + POINT(2)
            
         END DO
         
         W = ZZWIND2D ( N, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', 1, 0, OK )

      END DO


      DO N = 3, 10
         
C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Negatively oriented, regular polygon. '
     .   //      'N verts = #; POINT is not origin.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 2.D0
         POINT(2) = 3.D0

         DO J = 1, N

            THETA = - (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA) + POINT(1)
            VERTCS(2,J) = SIN(THETA) + POINT(2)
            
         END DO
         
         W = ZZWIND2D ( N, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', -1, 0, OK )

      END DO


C
C     Try some polygons with multiple wraps; path direction is 
C     positive.
C
      DO N = 3, 10         
C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Positively oriented, regular polygon. '
     .   //      'Wrap > 1. '
     .   //      'N verts = #; POINT is not 0.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 2.D0
         POINT(2) = 3.D0

         M = N*N

         DO J = 1, M

            THETA = (J-1) * ( 2*PI()/N )

            VERTCS(1,J) = COS(THETA) + POINT(1)
            VERTCS(2,J) = SIN(THETA) + POINT(2)
            
         END DO
         
         W = ZZWIND2D ( M, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', N, 0, OK )

      END DO

C
C     Try some irregualr polygons with multiple wraps; path 
C     direction is negative.
C
      DO N = 3, 10         
C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Negatively oriented, irregular polygon. '
     .   //      'Wrap > 1. '
     .   //      'N verts = #; POINT is not 0.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 2.D0
         POINT(2) = 3.D0

         M = N*N

         DO J = 1, M

            THETA = - (J-1) * ( 2*PI()/N )
C
C           Scale the length of the Jth vertex.
C
            VERTCS(1,J) = ( J * COS(THETA) ) + POINT(1)
            VERTCS(2,J) = ( J * SIN(THETA) ) + POINT(2)
            
         END DO
         
         W = ZZWIND2D ( M, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', -N, 0, OK )

      END DO


C
C     Try some irregualr polygons with multiple wraps; path 
C     direction is negative. POINT is outside polygon.
C
      DO N = 3, 10         
C
C---- Case -------------------------------------------------------------
C
         TITLE = 'Negatively oriented, irregular polygon. '
     .   //      'Wrap < -1. '
     .   //      'N verts = #; exterior POINT.'

         CALL REPMI ( TITLE, '#', N, TITLE )

         CALL TCASE ( TITLE )

         POINT(1) = 200.D0
         POINT(2) = 300.D0

         M = N*N

         DO J = 1, M

            THETA = - (J-1) * ( 2*PI()/N )
C
C           Scale the length of the Jth vertex.
C
            VERTCS(1,J) =   J * COS(THETA) 
            VERTCS(2,J) =   J * SIN(THETA) 
            
         END DO
         
         W = ZZWIND2D ( M, VERTCS, POINT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'W', W, '=', 0, 0, OK )

      END DO


      CALL T_SUCCESS ( OK )
      RETURN
      END




