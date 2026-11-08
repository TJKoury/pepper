C$Procedure F_IOVCMP ( IOVCMP tests )
 
      SUBROUTINE F_IOVCMP ( OK )
 
C$ Abstract
C
C     Exercise the compressed inverse order vector routine IOVCMP.
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
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the DSKBRIEF supporting routine IOVCMP.
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
C-    TSPICE Version 1.0.0, 08-FEB-2017 (NJB)
C
C        Original version 02-SEP-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CARDD

C
C     Local Parameters 
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      INTEGER               DBSIZE
      PARAMETER           ( DBSIZE = 100 )

C
C     Local Variables 
C  
      CHARACTER*(LNSIZE)    TITLE
  
      DOUBLE PRECISION      DARRAY ( DBSIZE )
      DOUBLE PRECISION      DARRY2 ( DBSIZE )
      DOUBLE PRECISION      DARRY3 ( DBSIZE )
      DOUBLE PRECISION      DSET   ( LBCELL : DBSIZE )
      DOUBLE PRECISION      DVAL

      INTEGER               I
      INTEGER               IORDER ( DBSIZE )
      INTEGER               INVORD ( DBSIZE )
      INTEGER               J
      INTEGER               K
      INTEGER               NDIM
      INTEGER               TO
      INTEGER               RNGMAX
      INTEGER               XORDER ( DBSIZE )

C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_IOVCMP' )



C**********************************************************************
C
C     IOVCMP normal cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Trivial case: ordered input set.' 

      CALL TCASE ( TITLE )
 
      NDIM = 100

      DO I = 1, NDIM
         DARRAY(I) = DBLE(I)
      END DO

      CALL IOVCMP ( DARRAY, NDIM, IORDER, INVORD, RNGMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The order vector and its inverse should both be 
C     the identity permutation.
C
      DO I = 1, NDIM
         XORDER(I) = I
      END DO

      CALL CHCKAI ( 'IORDER', IORDER, '=', XORDER, NDIM, OK )
      CALL CHCKAI ( 'INVORD', INVORD, '=', XORDER, NDIM, OK )

C
C     The maximum element of the inverse order vector is 
C     supposed to be NDIM.
C     
      CALL CHCKSI ( 'RNGMAX', RNGMAX, '=', NDIM, 0, OK )
      NDIM = 100


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Reverse-ordered input set.' 

      CALL TCASE ( TITLE )

      DO I = 1, NDIM
         DARRAY(I) = -DBLE(I)
      END DO

      CALL IOVCMP ( DARRAY, NDIM, IORDER, INVORD, RNGMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The order vector and its inverse should both be 
C     the reverse permutation.
C
      DO I = 1, NDIM
         XORDER(I) = NDIM+1-I
      END DO

      CALL CHCKAI ( 'IORDER', IORDER, '=', XORDER, NDIM, OK )
      CALL CHCKAI ( 'INVORD', INVORD, '=', XORDER, NDIM, OK )

C
C     The maximum element of the inverse order vector is 
C     supposed to be NDIM.
C     
      CALL CHCKSI ( 'RNGMAX', RNGMAX, '=', NDIM, 0, OK )

 
C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Ordered input set with paired, duplicate elements.'

      CALL TCASE ( TITLE )
 
      NDIM = 100

      DO I = 1, NDIM, 2
         DARRAY(I  ) = DBLE  (I)
         DARRAY(I+1) = DARRAY(I)
      END DO

      CALL IOVCMP ( DARRAY, NDIM, IORDER, INVORD, RNGMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Because the shell sort isn't stable, we don't know what
C     the expected order vector is. But we do know that we 
C     should be able to re-order the d.p. array using the 
C     order vector and obtain an ordered set. 
C
      CALL MOVED  ( DARRAY, NDIM, DARRY2 )
      CALL REORDD ( IORDER, NDIM, DARRY2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the expected set.
C
      CALL MOVED  ( DARRAY, NDIM, DARRY3 )
      CALL SHELLD (         NDIM, DARRY3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     This test actually checks IORDER.
C
      CALL CHCKAD ( 'DARRY2/IORDER', DARRY2, '=', 
     .                               DARRY3, NDIM, 0.D0, OK )

C
C     Now check the compressed inverse order vector.
C
C     Make a set out of the original array.
C
      CALL MOVED  ( DARRAY, NDIM, DSET(1) )

      CALL VALIDD ( NDIM, NDIM, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The compressed inverse order vector should map each index of
C     DARRAY to the index of an element in DSET containing the
C     same value as that located at the original index.
C
      DO I = 1, NDIM

         DVAL = DSET(  INVORD(I) )
         
         CALL CHCKSD ( 'DVAL', DVAL, '=', DARRAY(I), 0.D0, OK )

      END DO

C
C     The maximum element of the inverse order vector is 
C     supposed to be the cardinality of DSET.
C     
      CALL CHCKSI ( 'RNGMAX', RNGMAX, '=', CARDD(DSET), 0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Ordered input set with varying numbers of duplicate '
     .//       'elements.'

      CALL TCASE ( TITLE )
 
      NDIM = 30

      I  = 1
      TO = 1
      
      DO WHILE ( TO .LE. NDIM )
C
C        Let the number of duplicates range from 1 : 7.
C
         J = MIN( NDIM+1-I, MOD(I-1,7)+1 )

C
C        Fill in array elements TO : TO+J-1.
C
         DO K = 1, J
            DARRAY(TO+K-1) = DBLE(I)
         END DO

         TO = TO + J
         I  = I  + 1

      END DO


      CALL IOVCMP ( DARRAY, NDIM, IORDER, INVORD, RNGMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Because the shell sort isn't stable, we don't know what
C     the expected order vector is. But we do know that we 
C     should be able to re-order the d.p. array using the 
C     order vector and obtain an ordered set. 
C
      CALL MOVED  ( DARRAY, NDIM, DARRY2 )
      CALL REORDD ( IORDER, NDIM, DARRY2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the expected set.
C
      CALL MOVED  ( DARRAY, NDIM, DARRY3 )
      CALL SHELLD (         NDIM, DARRY3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     This test actually checks IORDER.
C
      CALL CHCKAD ( 'DARRY2/IORDER', DARRY2, '=', 
     .                               DARRY3, NDIM, 0.D0, OK )

C
C     Now check the compressed inverse order vector.
C
C     Make a set out of the original array.
C
      CALL MOVED  ( DARRAY, NDIM, DSET(1) )

      CALL VALIDD ( NDIM, NDIM, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The compressed inverse order vector should map each index of
C     DARRAY to the index of an element in DSET containing the
C     same value as that located at the original index.
C
      DO I = 1, NDIM

         DVAL = DSET(  INVORD(I) )
         
         CALL CHCKSD ( 'DVAL', DVAL, '=', DARRAY(I), 0.D0, OK )

      END DO

C
C     The maximum element of the inverse order vector is 
C     supposed to be the cardinality of DSET.
C     
      CALL CHCKSI ( 'RNGMAX', RNGMAX, '=', CARDD(DSET), 0, OK )


C
C --- Case: ------------------------------------------------------
C 
      TITLE =  'Ordered input set with varying numbers of duplicate '
     .//       'elements. Negate the input set values from the '
     .//       'previous case.'

      CALL TCASE ( TITLE )
 
      NDIM = 30

      I  = 1
      TO = 1
      
      DO WHILE ( TO .LE. NDIM )
C
C        Let the number of duplicates range from 1 : 7.
C
         J = MIN( NDIM+1-I, MOD(I-1,7)+1 )

C
C        Fill in array elements TO : TO+J-1.
C
         DO K = 1, J
            DARRAY(TO+K-1) = -DBLE(I)
         END DO

         TO = TO + J
         I  = I  + 1

      END DO


      CALL IOVCMP ( DARRAY, NDIM, IORDER, INVORD, RNGMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Because the shell sort isn't stable, we don't know what
C     the expected order vector is. But we do know that we 
C     should be able to re-order the d.p. array using the 
C     order vector and obtain an ordered set. 
C
      CALL MOVED  ( DARRAY, NDIM, DARRY2 )
      CALL REORDD ( IORDER, NDIM, DARRY2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create the expected set.
C
      CALL MOVED  ( DARRAY, NDIM, DARRY3 )
      CALL SHELLD (         NDIM, DARRY3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     This test actually checks IORDER.
C
      CALL CHCKAD ( 'DARRY2/IORDER', DARRY2, '=', 
     .                               DARRY3, NDIM, 0.D0, OK )

C
C     Now check the compressed inverse order vector.
C
C     Make a set out of the original array.
C
      CALL MOVED  ( DARRAY, NDIM, DSET(1) )

      CALL VALIDD ( NDIM, NDIM, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The compressed inverse order vector should map each index of
C     DARRAY to the index of an element in DSET containing the
C     same value as that located at the original index.
C
      DO I = 1, NDIM

         DVAL = DSET(  INVORD(I) )
         
         CALL CHCKSD ( 'DVAL', DVAL, '=', DARRAY(I), 0.D0, OK )

      END DO

C
C     The maximum element of the inverse order vector is 
C     supposed to be the cardinality of DSET.
C     
      CALL CHCKSI ( 'RNGMAX', RNGMAX, '=', CARDD(DSET), 0, OK )


C**********************************************************************
C
C     IOVCMP error cases
C
C**********************************************************************

C
C     None so far.
C


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


