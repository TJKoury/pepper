!$Procedure  ZZEKQCNJ ( Private: EK, read conjunction sizes from query )

      SUBROUTINE ZZEKQCNJ ( EQRYI, N, SIZE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read conjunction sizes from an encoded EK query.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekquery.inc'
      INCLUDE 'ekbool.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               EQRYI ( LBCELL : * )
      INTEGER               N
      INTEGER               SIZE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     N          I   Index within FROM clause of table name to read.
!     SIZE       O   Size of Nth conjunction in WHERE clause.
!
! $ Detailed_Input
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     N              is the index, within the FROM clause of the query,
!                    of the table whose name is to be fetched.
!
! $ Detailed_Output
!
!     SIZE           is the size of the Nth conjunction of
!                    constraints in the input encoded query.  The size
!                    applies to the constraints after `normalization'.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input query is not initialized, the error will be
!         diagnosed by routines called by this routine.  The outputs
!         will not be modified.
!
!     2)  If the input query has not been parsed, the error
!         SPICE(UNPARSEDQUERY) will be signaled.  The outputs
!         will not be modified.
!
!     3)  If the index N is less than 1 or greater than the number of
!         conjunctions in the query, the error SPICE(INVALIDINDEX)
!         will be signaled.  The outputs will not be modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The call
!
!        CALL ZZEKREQI (  EQRYI,  'NUM_CONJUNCTIONS',  N  )
!
!     may be used to get the conjunction count from an encoded query.
!
!     This routine assumes that encoded EK query architecture version
!     1 is to be used with the query to be initialized; this routine
!     will not work with any other architecture version.
!
! $ Examples
!
!     See EKSRCH.
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
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local variables
!
      INTEGER               IPARSE
      INTEGER               LOC
      INTEGER               NCNJ
      INTEGER               NCNS
      INTEGER               NTAB

!
!     Use discovery check-in.
!

      CALL ZZEKREQI ( EQRYI, 'PARSED', IPARSE )

      IF ( FAILED() ) THEN
         RETURN
      END IF

      IF ( IPARSE .EQ. IFALSE ) THEN

         CALL CHKIN  ( 'ZZEKQCNJ'                                )
         CALL SETMSG ( 'Encoded query has not yet been parsed.'  )
         CALL SIGERR ( 'SPICE(UNPARSEDQUERY)'                    )
         CALL CHKOUT ( 'ZZEKQCNJ'                                )
         RETURN

      END IF

      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',         NTAB   )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONJUNCTIONS',   NCNJ   )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONSTRAINTS',    NCNS   )


      IF (  ( N .LT. 1 ) .OR. ( N .GT. NCNJ )  ) THEN

         CALL CHKIN  ( 'ZZEKQCNJ'                                  )
         CALL SETMSG ( 'Table index # is out of valid range 1:#.'  )
         CALL ERRINT ( '#',  N                                     )
         CALL ERRINT ( '#',  NCNJ                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
         CALL CHKOUT ( 'ZZEKQCNJ'                                  )
         RETURN

      END IF

!
!     Compute the location of the requested conjunction size value.
!
      LOC    =     EQVBAS &
                 + NTAB  *  2  *  EQVDSZ &
                 + NCNS        *  EQCDSZ &
                 + N

      SIZE   =  EQRYI ( LOC )


      RETURN
      END
