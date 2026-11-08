C$Procedure F_SWAPAI ( Test the SPICELIB routine SWAPAI )
 
      SUBROUTINE F_SWAPAI ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB sub-array swapping routine SWAPAI.
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
C     This routine tests the SPICELIB routine SWAPAI.
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
C-    TSPICE Version 1.0.0, 14-OCT-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C


C
C     Local Parameters
C
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               LARGE
      PARAMETER           ( LARGE  = 1000 )

      INTEGER               NUMCAS
      PARAMETER           ( NUMCAS = 1000 )

      INTEGER               SMALL
      PARAMETER           ( SMALL  = 10 )

C
C     Local Variables
C
      CHARACTER*(MSGLEN)    TITLE

      INTEGER               ARRAY0 ( LARGE )
      INTEGER               ARRAY1 ( LARGE )
      INTEGER               I
      INTEGER               LOCM
      INTEGER               LOCN
      INTEGER               M
      INTEGER               N
      INTEGER               SIZE
      INTEGER               START
      INTEGER               WORK   ( LARGE )
      INTEGER               XARRAY ( LARGE )

C
C     Saved values
C

C
C     Initial values
C



C
C     Open the test family.
C
      CALL TOPEN ( 'F_SWAPAI' )


C
C     We'll do an exhaustive set of tests on a small array.
C     
      CALL CLEARI ( LARGE, ARRAY0 )

      SIZE = SMALL

      DO I = 1, SIZE         
         ARRAY0(I) = I
      END DO

C
C     LOCN is the start of the "upper" slice.
C
      DO LOCN = 1, SIZE
C
C        N is the size of the upper slice.
C
         DO N = 0, SIZE-LOCN+1
C
C           LOCM is the start of the "lower" slice.
C
            START = LOCN + MAX ( 1, N )

            DO LOCM = START, SIZE
C              
C              M is the size of the lower slice.
C
               DO M = 0, SIZE-LOCM+1
C
C --- Case: ------------------------------------------------------
C 
                  TITLE = 'Case: N = #; LOCN = #; M = #; LOCM = #.'

                  CALL REPMI  ( TITLE, '#', N,    TITLE )      
                  CALL REPMI  ( TITLE, '#', LOCN, TITLE )      
                  CALL REPMI  ( TITLE, '#', M,    TITLE )      
                  CALL REPMI  ( TITLE, '#', LOCM, TITLE )      
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE ( TITLE )
      
C
C                 Make two copies of the initial array.
C
                  CALL MOVEI ( ARRAY0, SIZE, ARRAY1 )
                  CALL MOVEI ( ARRAY0, SIZE, XARRAY )

C
C                 Swap the array slices indicated by LOCN, N, LOCM, 
C                 and M.
C
                  CALL SWAPAI ( N, LOCN, M, LOCM, ARRAY1 )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Produce our expected array.
C
                  CALL T_SWAPAI ( SIZE, N,    LOCN, 
     .                            M,    LOCM, XARRAY, WORK )

C
C                 Test our results.
C
                  CALL CHCKAI ( 'swapped array (0)', 
     .                           ARRAY1,  '=', 
     .                           XARRAY,  SIZE,  OK  )


C
C                 Now we'll repeat the test with the order of
C                 the arguments swapped.
C
                  CALL MOVEI ( ARRAY0, SIZE, ARRAY1 )
                  CALL MOVEI ( ARRAY0, SIZE, XARRAY )

C
C                 Swap the array slices indicated by LOCN, N, LOCM, 
C                 and M.
C
                  CALL SWAPAI ( M, LOCM, N, LOCN, ARRAY1 )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Produce our expected array.
C
                  CALL T_SWAPAI ( SIZE, M,    LOCM,  
     .                            N,    LOCN, XARRAY, WORK )

C
C                 Test our results.
C
                  CALL CHCKAI ( 'swapped array (1)', 
     .                           ARRAY1,  '=', 
     .                           XARRAY,  SIZE,  OK  )
               END DO

            END DO


         END DO

      END DO
      

C
C     Now for some error handling tests.  
C      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SWAPAI: overlapping array slices.' )
 
      CALL SWAPAI ( 2, 1, 2, 1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTDISTINCT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SWAPAI: non-positive value of LOCN, LOCM' )
 
      CALL SWAPAI (  2,  0,  2,  1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

      CALL SWAPAI (  2, -1,  2,  1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

      CALL SWAPAI (  2,  1,  2, -1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

      CALL SWAPAI (  2,  1,  2,  0, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'SWAPAI: Negative value of N, M' )
 
      CALL SWAPAI ( -2,  0,  2,  1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARGUMENT)', OK )

      CALL SWAPAI (  2,  0, -2,  1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARGUMENT)', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


C
C     T_SWAPAI is a utility routine used for testing SWAPAI. T_SWAPAI
C     uses an alternate implementation of the swapping algorithm that
C     uses work space to build the output array:  the swapping
C     algorithm is quite simple when the constraint of in-place
C     operation is removed.
C
C     This routine does no error checking.
C
      SUBROUTINE T_SWAPAI ( SIZE, N, LOCN, M, LOCM, ARRAY, WORK )
      IMPLICIT NONE

      INTEGER               SIZE
      INTEGER               N
      INTEGER               LOCN
      INTEGER               M
      INTEGER               LOCM
      INTEGER               ARRAY ( * )
      INTEGER               WORK  ( * )

C
C     Local variables
C
      INTEGER               LOC
      INTEGER               LOWER
      INTEGER               NLOW
      INTEGER               NMOVE
      INTEGER               NUP
      INTEGER               TO
      INTEGER               UPPER


C
C     We'll build the output array in WORK; then we'll copy the
C     result back to ARRAY.
C
C     Identify the start indices of the "top" and "bottom" array slices
C     to be swapped.  We consider the lower addresses to be at the
C     "top" of the array.
C
      UPPER = MIN ( LOCN, LOCM )
      LOWER = MAX ( LOCN, LOCM )

      IF ( UPPER .EQ. LOCN ) THEN
         NUP  = N
         NLOW = M
      ELSE         
         NUP  = M
         NLOW = N
      END IF

C
C     Move the elements preceding UPPER into WORK.
C
      TO    = 1
      NMOVE = UPPER - 1
      CALL MOVEI ( ARRAY(1),      NMOVE,  WORK(TO) )

C
C     Move the elements in the lower slice into WORK.
C
      TO    = TO + NMOVE 
      NMOVE = NLOW
      CALL MOVEI ( ARRAY(LOWER),  NMOVE,  WORK(TO) )

C
C     Move the elements between the slices into WORK.
C
      TO     =  TO + NMOVE 
      NMOVE  =  LOWER - 1 - ( UPPER + NUP - 1 )

      CALL MOVEI ( ARRAY(UPPER+NUP),  NMOVE,  WORK(TO) )

C
C     Move the elements in the upper slice into WORK.
C
      TO     =  TO + NMOVE 
      NMOVE  =  NUP
      CALL MOVEI ( ARRAY(UPPER),  NMOVE,  WORK(TO) )

C
C     Move the elements below the lower slice into WORK.
C
      TO     =  TO + NMOVE 
      LOC    =  LOWER + NLOW
      NMOVE  =  SIZE - ( LOC - 1 )

      CALL MOVEI ( ARRAY(LOC),  NMOVE,  WORK(TO) )

C
C     Copy WORK into ARRAY.
C
      CALL MOVEI ( WORK, SIZE, ARRAY )

      END



