!$Procedure F_IOVCMP ( IOVCMP tests )

      SUBROUTINE F_IOVCMP ( OK )

! $ Abstract
!
!     Exercise the compressed inverse order vector routine IOVCMP.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the DSKBRIEF supporting routine IOVCMP.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Original version 02-SEP-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               CARDD

!
!     Local Parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      INTEGER               DBSIZE
      PARAMETER           ( DBSIZE = 100 )

!
!     Local Variables
!
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

!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_IOVCMP' )



!**********************************************************************
!
!     IOVCMP normal cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Trivial case: ordered input set.'

      CALL TCASE ( TITLE )

      NDIM = 100

      DO I = 1, NDIM
         DARRAY(I) = DBLE(I)
      END DO

      CALL IOVCMP ( DARRAY, NDIM, IORDER, INVORD, RNGMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The order vector and its inverse should both be
!     the identity permutation.
!
      DO I = 1, NDIM
         XORDER(I) = I
      END DO

      CALL CHCKAI ( 'IORDER', IORDER, '=', XORDER, NDIM, OK )
      CALL CHCKAI ( 'INVORD', INVORD, '=', XORDER, NDIM, OK )

!
!     The maximum element of the inverse order vector is
!     supposed to be NDIM.
!
      CALL CHCKSI ( 'RNGMAX', RNGMAX, '=', NDIM, 0, OK )
      NDIM = 100


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Reverse-ordered input set.'

      CALL TCASE ( TITLE )

      DO I = 1, NDIM
         DARRAY(I) = -DBLE(I)
      END DO

      CALL IOVCMP ( DARRAY, NDIM, IORDER, INVORD, RNGMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The order vector and its inverse should both be
!     the reverse permutation.
!
      DO I = 1, NDIM
         XORDER(I) = NDIM+1-I
      END DO

      CALL CHCKAI ( 'IORDER', IORDER, '=', XORDER, NDIM, OK )
      CALL CHCKAI ( 'INVORD', INVORD, '=', XORDER, NDIM, OK )

!
!     The maximum element of the inverse order vector is
!     supposed to be NDIM.
!
      CALL CHCKSI ( 'RNGMAX', RNGMAX, '=', NDIM, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Ordered input set with paired, duplicate elements.'

      CALL TCASE ( TITLE )

      NDIM = 100

      DO I = 1, NDIM, 2
         DARRAY(I  ) = DBLE  (I)
         DARRAY(I+1) = DARRAY(I)
      END DO

      CALL IOVCMP ( DARRAY, NDIM, IORDER, INVORD, RNGMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Because the shell sort isn't stable, we don't know what
!     the expected order vector is. But we do know that we
!     should be able to re-order the d.p. array using the
!     order vector and obtain an ordered set.
!
      CALL MOVED  ( DARRAY, NDIM, DARRY2 )
      CALL REORDD ( IORDER, NDIM, DARRY2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the expected set.
!
      CALL MOVED  ( DARRAY, NDIM, DARRY3 )
      CALL SHELLD (         NDIM, DARRY3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     This test actually checks IORDER.
!
      CALL CHCKAD ( 'DARRY2/IORDER', DARRY2, '=', &
                                     DARRY3, NDIM, 0.D0, OK )

!
!     Now check the compressed inverse order vector.
!
!     Make a set out of the original array.
!
      CALL MOVED  ( DARRAY, NDIM, DSET(1) )

      CALL VALIDD ( NDIM, NDIM, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The compressed inverse order vector should map each index of
!     DARRAY to the index of an element in DSET containing the
!     same value as that located at the original index.
!
      DO I = 1, NDIM

         DVAL = DSET(  INVORD(I) )

         CALL CHCKSD ( 'DVAL', DVAL, '=', DARRAY(I), 0.D0, OK )

      END DO

!
!     The maximum element of the inverse order vector is
!     supposed to be the cardinality of DSET.
!
      CALL CHCKSI ( 'RNGMAX', RNGMAX, '=', CARDD(DSET), 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Ordered input set with varying numbers of duplicate ' &
      //       'elements.'

      CALL TCASE ( TITLE )

      NDIM = 30

      I  = 1
      TO = 1

      DO WHILE ( TO .LE. NDIM )
!
!        Let the number of duplicates range from 1 : 7.
!
         J = MIN( NDIM+1-I, MOD(I-1,7)+1 )

!
!        Fill in array elements TO : TO+J-1.
!
         DO K = 1, J
            DARRAY(TO+K-1) = DBLE(I)
         END DO

         TO = TO + J
         I  = I  + 1

      END DO


      CALL IOVCMP ( DARRAY, NDIM, IORDER, INVORD, RNGMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Because the shell sort isn't stable, we don't know what
!     the expected order vector is. But we do know that we
!     should be able to re-order the d.p. array using the
!     order vector and obtain an ordered set.
!
      CALL MOVED  ( DARRAY, NDIM, DARRY2 )
      CALL REORDD ( IORDER, NDIM, DARRY2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the expected set.
!
      CALL MOVED  ( DARRAY, NDIM, DARRY3 )
      CALL SHELLD (         NDIM, DARRY3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     This test actually checks IORDER.
!
      CALL CHCKAD ( 'DARRY2/IORDER', DARRY2, '=', &
                                     DARRY3, NDIM, 0.D0, OK )

!
!     Now check the compressed inverse order vector.
!
!     Make a set out of the original array.
!
      CALL MOVED  ( DARRAY, NDIM, DSET(1) )

      CALL VALIDD ( NDIM, NDIM, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The compressed inverse order vector should map each index of
!     DARRAY to the index of an element in DSET containing the
!     same value as that located at the original index.
!
      DO I = 1, NDIM

         DVAL = DSET(  INVORD(I) )

         CALL CHCKSD ( 'DVAL', DVAL, '=', DARRAY(I), 0.D0, OK )

      END DO

!
!     The maximum element of the inverse order vector is
!     supposed to be the cardinality of DSET.
!
      CALL CHCKSI ( 'RNGMAX', RNGMAX, '=', CARDD(DSET), 0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Ordered input set with varying numbers of duplicate ' &
      //       'elements. Negate the input set values from the ' &
      //       'previous case.'

      CALL TCASE ( TITLE )

      NDIM = 30

      I  = 1
      TO = 1

      DO WHILE ( TO .LE. NDIM )
!
!        Let the number of duplicates range from 1 : 7.
!
         J = MIN( NDIM+1-I, MOD(I-1,7)+1 )

!
!        Fill in array elements TO : TO+J-1.
!
         DO K = 1, J
            DARRAY(TO+K-1) = -DBLE(I)
         END DO

         TO = TO + J
         I  = I  + 1

      END DO


      CALL IOVCMP ( DARRAY, NDIM, IORDER, INVORD, RNGMAX )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Because the shell sort isn't stable, we don't know what
!     the expected order vector is. But we do know that we
!     should be able to re-order the d.p. array using the
!     order vector and obtain an ordered set.
!
      CALL MOVED  ( DARRAY, NDIM, DARRY2 )
      CALL REORDD ( IORDER, NDIM, DARRY2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the expected set.
!
      CALL MOVED  ( DARRAY, NDIM, DARRY3 )
      CALL SHELLD (         NDIM, DARRY3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     This test actually checks IORDER.
!
      CALL CHCKAD ( 'DARRY2/IORDER', DARRY2, '=', &
                                     DARRY3, NDIM, 0.D0, OK )

!
!     Now check the compressed inverse order vector.
!
!     Make a set out of the original array.
!
      CALL MOVED  ( DARRAY, NDIM, DSET(1) )

      CALL VALIDD ( NDIM, NDIM, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The compressed inverse order vector should map each index of
!     DARRAY to the index of an element in DSET containing the
!     same value as that located at the original index.
!
      DO I = 1, NDIM

         DVAL = DSET(  INVORD(I) )

         CALL CHCKSD ( 'DVAL', DVAL, '=', DARRAY(I), 0.D0, OK )

      END DO

!
!     The maximum element of the inverse order vector is
!     supposed to be the cardinality of DSET.
!
      CALL CHCKSI ( 'RNGMAX', RNGMAX, '=', CARDD(DSET), 0, OK )


!**********************************************************************
!
!     IOVCMP error cases
!
!**********************************************************************

!
!     None so far.
!


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


