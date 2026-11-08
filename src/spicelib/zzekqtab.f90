!$Procedure  ZZEKQTAB ( Private: EK, read table names from query )

      SUBROUTINE ZZEKQTAB ( EQRYI, EQRYC, N, TABLE, ALIAS )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read table names and aliases from an encoded EK query.
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
      CHARACTER*(*)         EQRYC
      INTEGER               N
      CHARACTER*(*)         TABLE
      CHARACTER*(*)         ALIAS

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     EQRYC      I   Character component of query.
!     N          I   Index within FROM clause of table name to read.
!     TABLE      O   Name of Nth table in FROM clause.
!     ALIAS      O   Alias of Nth table in FROM clause.
!
! $ Detailed_Input
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     N              is the index, within the FROM clause of the query,
!                    of the table whose name is to be fetched.
!
! $ Detailed_Output
!
!     TABLE          is the name of the Nth table in the FROM clause of
!                    the input encoded query.
!
!     ALIAS          is the alias of TABLE.  If no alias for TABLE is
!                    present, ALIAS is returned blank.
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
!         tables in the FROM clause, the error SPICE(INVALIDINDEX)
!         will be signaled.  The outputs
!         will not be modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The call
!
!        CALL ZZEKREQI (  EQRYI,  'NUM_TABLES',  N  )
!
!     may be used to get the FROM table count from an encoded query.
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
      INTEGER               AB
      INTEGER               AE
      INTEGER               BASE
      INTEGER               BUFLEN
      INTEGER               IPARSE
      INTEGER               NTAB
      INTEGER               TB
      INTEGER               TE

!
!     Use discovery check-in.
!

      CALL ZZEKREQI ( EQRYI, 'PARSED', IPARSE )

      IF ( FAILED() ) THEN
         RETURN
      END IF

      IF ( IPARSE .EQ. IFALSE ) THEN

         CALL CHKIN  ( 'ZZEKQTAB'                                )
         CALL SETMSG ( 'Encoded query has not yet been parsed.'  )
         CALL SIGERR ( 'SPICE(UNPARSEDQUERY)'                    )
         CALL CHKOUT ( 'ZZEKQTAB'                                )
         RETURN

      END IF

      CALL ZZEKREQI ( EQRYI, 'CHR_BUF_SIZE', BUFLEN )
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',   NTAB   )


      IF (  ( N .LT. 1 ) .OR. ( N .GT. NTAB )  ) THEN

         CALL CHKIN  ( 'ZZEKQTAB'                                  )
         CALL SETMSG ( 'Table index # is out of valid range 1:#.'  )
         CALL ERRINT ( '#',  N                                     )
         CALL ERRINT ( '#',  NTAB                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
         CALL CHKOUT ( 'ZZEKQTAB'                                  )
         RETURN

      END IF

!
!     Get the Nth table and alias from the query.  The table
!     descriptor lies beyond the fixed-size portion of the query, as
!     well as the (N-1) previous descriptors, each one of which has
!     size 2*EQVDSZ.
!
      BASE   =  EQVBAS + (N-1)*2*EQVDSZ

      TB     =  EQRYI ( BASE + EQBSTR )
      TE     =  EQRYI ( BASE + EQESTR )

      IF (       ( TB .GT. 0      ) &
           .AND. ( TE .GT. 0      ) &
           .AND. ( TB .LE. BUFLEN ) &
           .AND. ( TE .LE. BUFLEN ) &
           .AND. ( TB .LE. TE     )  ) THEN

         TABLE = EQRYC(TB:TE)

      ELSE
!
!        We should never see invalid pointers in a parsed, encoded
!        query, but let's not take chances.
!
         CALL CHKIN  ( 'ZZEKQTAB'                                )
         CALL SETMSG ( 'Invalid string bounds #:# for table #.'  )
         CALL ERRINT ( '#',  TB                                  )
         CALL ERRINT ( '#',  TE                                  )
         CALL ERRINT ( '#',  N                                   )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKQTAB'                                )
         RETURN

      END IF

!
!     Same deal for the alias, except that the begin pointer is
!     set to zero if there's no alias.
!
      AB     =  EQRYI ( BASE + EQVDSZ + EQBSTR )
      AE     =  EQRYI ( BASE + EQVDSZ + EQESTR )

      IF ( AB .GT. 0 ) THEN

         IF (       ( AE .GT. 0      ) &
              .AND. ( AB .LE. BUFLEN ) &
              .AND. ( AE .LE. BUFLEN ) &
              .AND. ( AB .LE. AE     )  ) THEN

            ALIAS  = EQRYC(AB:AE)

         ELSE
!
!           If the first pointer is non-zero, both pointers should have
!           been valid.
!
            CALL CHKIN  ( 'ZZEKQTAB'                                   )
            CALL SETMSG ( 'Invalid string bounds #:# for the alias '  // &
                          'of table #.'                                )
            CALL ERRINT ( '#',  AB                                     )
            CALL ERRINT ( '#',  AE                                     )
            CALL ERRINT ( '#',  N                                      )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZEKQTAB'                                   )
            RETURN

         END IF

      ELSE
!
!        No alias was supplied.
!
         ALIAS  =  ' '

      END IF

      RETURN
      END
