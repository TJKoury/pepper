!$Procedure  ZZEKQSEL ( Private: EK, read SELECT columns from query )

      SUBROUTINE ZZEKQSEL ( EQRYI,  EQRYC,   N,       LXBEG,  LXEND, &
                            TABLE,  TABIDX,  COLUMN,  COLIDX        )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a specified SELECT clause table and column name from an
!     encoded EK query.
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
      INTEGER               LXBEG
      INTEGER               LXEND
      CHARACTER*(*)         TABLE
      INTEGER               TABIDX
      CHARACTER*(*)         COLUMN
      INTEGER               COLIDX

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     EQRYC      I   Character component of query.
!     N          I   Index within SELECT clause of table name to read.
!     LXBEG      O   Begin index in query of selected item.
!     LXEND      O   End index in query of selected item.
!     TABLE      O   Name of table qualifying Nth SELECT column.
!     TABIDX     O   Index of TABLE in FROM clause, if known.
!     COLUMN     O   Nth column in SELECT clause of query.
!     TABIDX     O   Index of column in TABLE, if known.
!
! $ Detailed_Input
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     N              is the index, within the SELECT clause of the
!                    query, of the table whose name is to be fetched.
!
! $ Detailed_Output
!
!     LXBEG,
!     LXEND          are, respectively, the begin and end indices in
!                    the original query of the selected item.
!
!     TABLE          is the table name or alias associated with the Nth
!                    column in the SELECT clause of an the input
!                    encoded query.  If the Nth column is unqualified,
!                    TABLE is returned blank.
!
!     TABIDX         is the ordinal position in the FROM clause of the
!                    input query of the table containing the Nth order-
!                    by column.  TABIDX is meaningful only if name
!                    resolution has not been performed on the input
!                    query; otherwise, TABIDX is returned as zero.
!
!     COLUMN         is the name of the Nth column in the SELECT
!                    clause of an the input encoded query.
!
!     COLIDX         is the ordinal position of the Nth column in the
!                    SELECT clause with respect to the virtual table
!                    designated by TABLE.  This index is available only
!                    if the query has already had names resolved;
!                    otherwise, COLIDX is returned as zero.
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
!         columns in the SELECT clause, the error SPICE(INVALIDINDEX)
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
!        CALL ZZEKREQI (  EQRYI,  'NUM_SELECT_COLS',  N  )
!
!     may be used to get the SELECT column count from an encoded
!     query.
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
      INTEGER               BASE
      INTEGER               BUFLEN
      INTEGER               CB
      INTEGER               CE
      INTEGER               IPARSE
      INTEGER               NCNJ
      INTEGER               NCNS
      INTEGER               NORD
      INTEGER               NSEL
      INTEGER               NTAB
      INTEGER               RESOLV
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

         CALL CHKIN  ( 'ZZEKQSEL'                                )
         CALL SETMSG ( 'Encoded query has not yet been parsed.'  )
         CALL SIGERR ( 'SPICE(UNPARSEDQUERY)'                    )
         CALL CHKOUT ( 'ZZEKQSEL'                                )
         RETURN

      END IF


      CALL ZZEKREQI ( EQRYI,  'NUM_SELECT_COLS',   NSEL   )

      IF (  ( N .LT. 1 ) .OR. ( N .GT. NSEL )  ) THEN

         CALL CHKIN  ( 'ZZEKQSEL'                                  )
         CALL SETMSG ( 'Column index # is out of valid range 1:#.' )
         CALL ERRINT ( '#',  N                                     )
         CALL ERRINT ( '#',  NSEL                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
         CALL CHKOUT ( 'ZZEKQSEL'                                  )
         RETURN

      END IF


      CALL ZZEKREQI ( EQRYI,  'NUM_TABLES',         NTAB   )
      CALL ZZEKREQI ( EQRYI,  'NUM_CONJUNCTIONS',   NCNJ   )
      CALL ZZEKREQI ( EQRYI,  'NUM_CONSTRAINTS',    NCNS   )
      CALL ZZEKREQI ( EQRYI,  'NUM_ORDERBY_COLS',   NORD   )
      CALL ZZEKREQI ( EQRYI,  'CHR_BUF_SIZE',       BUFLEN )

!
!     The lexeme begin and end values start out as invalid values.
!     We'll set these when we discover what form the SELECT item has.
!

!
      LXBEG  =  0
      LXEND  =  0

!
!     Get the Nth table and column from the query.  The table
!     descriptor lies beyond the fixed-size portion of the query, the
!     conjunction size list, the constraint descriptors, the order-by
!     column descriptors, as well as the (N-1) previous SELECT column
!     descriptors.
!
      BASE   =        EQVBAS &
                  +   NTAB * 2 * EQVDSZ &
                  +   NCNJ &
                  +   NCNS     * EQCDSZ &
                  +   NORD     * EQODSZ &
                  +   (N-1)    * EQSDSZ


!
!     Pick up the column name first.
!
      CB     =  EQRYI ( BASE + EQVDSZ + EQBSTR )
      CE     =  EQRYI ( BASE + EQVDSZ + EQESTR )

      IF (       ( CB .GT. 0      ) &
           .AND. ( CE .GT. 0      ) &
           .AND. ( CB .LE. BUFLEN ) &
           .AND. ( CE .LE. BUFLEN ) &
           .AND. ( CB .LE. CE     )  ) THEN

         COLUMN = EQRYC(CB:CE)

!
!        The end of the column name is always the end of the SELECT
!        item, at least until we handle more general expressions.
!
         LXEND  =  EQRYI ( BASE + EQVDSZ + EQELEX )

      ELSE
!
!        We should never see invalid pointers in a parsed, encoded
!        query, but let's not take chances.
!
         CALL CHKIN  ( 'ZZEKQSEL'                                )
         CALL SETMSG ( 'Invalid string bounds #:# for column #.' )
         CALL ERRINT ( '#',  CB                                  )
         CALL ERRINT ( '#',  CE                                  )
         CALL ERRINT ( '#',  N                                   )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKQSEL'                                )
         RETURN

      END IF

!
!     Same deal for the qualifying table or alias, except that the begin
!     pointer is set to zero if there's no name.
!
      TB     =  EQRYI ( BASE + EQBSTR )
      TE     =  EQRYI ( BASE + EQESTR )

      IF ( TB .GT. 0 ) THEN

         IF (       ( TE .GT. 0      ) &
              .AND. ( TB .LE. BUFLEN ) &
              .AND. ( TE .LE. BUFLEN ) &
              .AND. ( TB .LE. TE     )  ) THEN

            TABLE  = EQRYC(TB:TE)

!
!           The start position of the table name is the start of
!           the SELECT item.
!
            LXBEG  =  EQRYI ( BASE + EQBLEX )


         ELSE
!
!           If the first pointer is non-zero, both pointers should have
!           been valid.
!
            CALL CHKIN  ( 'ZZEKQSEL'                                   )
            CALL SETMSG ( 'Invalid string bounds #:# for the table '  // &
                          'qualifying column #.'                       )
            CALL ERRINT ( '#',  TB                                     )
            CALL ERRINT ( '#',  TE                                     )
            CALL ERRINT ( '#',  N                                      )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZEKQSEL'                                   )
            RETURN

         END IF


      ELSE
!
!        No table was supplied.
!
         TABLE  =  ' '

!
!        The start position of the column name is the start of
!        the SELECT item.
!
         LXBEG  =  EQRYI ( BASE + EQVDSZ + EQBLEX )

      END IF

!
!     If names have been resolved already, we can determine the index
!     of the table to which the specified order-by column belongs.
!
      CALL ZZEKREQI ( EQRYI,  'NAMES_RESOLVED',   RESOLV )

      IF ( RESOLV .EQ. ITRUE ) THEN
!
!        The qualifying table's index in the FROM clause is available.
!        So is the index of the column within the table.
!
         TABIDX  =  EQRYI( BASE + EQTORD              )
         COLIDX  =  EQRYI( BASE + EQSCOL - 1 + EQCIDX )

      ELSE

         TABIDX  =  0
         COLIDX  =  0

      END IF

      RETURN
      END
