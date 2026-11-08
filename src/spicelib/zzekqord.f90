!$Procedure  ZZEKQORD ( Private: EK, read order-by columns from query )

      SUBROUTINE ZZEKQORD ( EQRYI,  EQRYC, N, TABLE, TABIDX, COLUMN, &
                            COLIDX, SENSE                            )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read a specified order-by table and column name, along with the
!     corresponding order sense, from an encoded EK query.
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
      INTEGER               TABIDX
      CHARACTER*(*)         COLUMN
      INTEGER               COLIDX
      INTEGER               SENSE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI      I   Integer component of query.
!     EQRYC      I   Character component of query.
!     N          I   Index within FROM clause of table name to read.
!     TABLE      O   Name of table qualifying Nth ORDER BY column.
!     TABIDX     O   Index of TABLE in FROM clause, if known.
!     COLUMN     O   Nth column in ORDER BY clause of query.
!     TABIDX     O   Index of column in TABLE, if known.
!     SENSE      O   Code giving order sense for Nth column.
!
! $ Detailed_Input
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     N              is the index, within the ORDER BY clause of the
!                    query, of the table whose name is to be fetched.
!
! $ Detailed_Output
!
!     TABLE          is the table name or alias associated with the Nth
!                    column in the ORDER BY clause of an the input
!                    encoded query.  If the Nth column is unqualified,
!                    TABLE is returned blank.
!
!     TABIDX         is the ordinal position in the FROM clause of the
!                    input query of the table containing the Nth order-
!                    by column.  TABIDX is meaningful only if name
!                    resolution has not been performed on the input
!                    query; otherwise, TABIDX is returned as zero.
!
!     COLUMN         is the name of the Nth column in the ORDER BY
!                    clause of an the input encoded query.
!
!     COLIDX         is the ordinal position of the Nth column in the
!                    ORDER BY clause with respect to the virtual table
!                    designated by TABLE.  This index is available only
!                    if the query has already had names resolved;
!                    otherwise, COLIDX is returned as zero.
!
!     SENSE          is an integer code giving the ordering sense to
!                    use with the specified column.  The possible values
!                    of SENSE are EQASND, which indicates that the
!                    order sense is ascending, and EQDSND, which
!                    indicates that the order sense is descending.
!                    `Ascending order' means that the order relation
!                    defined by the indicated column orders rows
!                    according to the order of elements in the
!                    indicated order-by column; `descending order' means
!                    that the order relation orders columns in the
!                    reverse of ascending order.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input query is not initialized, the error will be
!         diagnosed by routines called by this routine. The outputs
!         will not be modified.
!
!     2)  If the input query has not been parsed, the error
!         SPICE(UNPARSEDQUERY) will be signaled. The outputs
!         will not be modified.
!
!     3)  If the index N is less than 1 or greater than the number of
!         columns in the ORDER BY clause, the error SPICE(INVALIDINDEX)
!         will be signaled. The outputs will not be modified.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The call
!
!        CALL ZZEKREQI (  EQRYI,  'NUM_ORDERBY_COLS',  N  )
!
!     may be used to get the ORDER BY column count from an encoded
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
!-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
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

         CALL CHKIN  ( 'ZZEKQORD'                                )
         CALL SETMSG ( 'Encoded query has not yet been parsed.'  )
         CALL SIGERR ( 'SPICE(UNPARSEDQUERY)'                    )
         CALL CHKOUT ( 'ZZEKQORD'                                )
         RETURN

      END IF


      CALL ZZEKREQI ( EQRYI,  'NUM_ORDERBY_COLS',   NORD   )

      IF (  ( N .LT. 1 ) .OR. ( N .GT. NORD )  ) THEN

         CALL CHKIN  ( 'ZZEKQORD'                                  )
         CALL SETMSG ( 'Column index # is out of valid range 1:#.' )
         CALL ERRINT ( '#',  N                                     )
         CALL ERRINT ( '#',  NORD                                  )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
         CALL CHKOUT ( 'ZZEKQORD'                                  )
         RETURN

      END IF


      CALL ZZEKREQI ( EQRYI,  'NUM_TABLES',         NTAB   )
      CALL ZZEKREQI ( EQRYI,  'NUM_CONJUNCTIONS',   NCNJ   )
      CALL ZZEKREQI ( EQRYI,  'NUM_CONSTRAINTS',    NCNS   )
      CALL ZZEKREQI ( EQRYI,  'CHR_BUF_SIZE',       BUFLEN )

!
!     Get the Nth table and column from the query.  The table
!     descriptor lies beyond the fixed-size portion of the query, the
!     conjunction size list, and the constraint descriptors, as
!     well as the (N-1) previous order-by column descriptors.
!
      BASE   =        EQVBAS &
                  +   NTAB * 2 * EQVDSZ &
                  +   NCNJ &
                  +   NCNS     * EQCDSZ &
                  +   (N-1)    * EQODSZ


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

      ELSE
!
!        We should never see invalid pointers in a parsed, encoded
!        query, but let's not take chances.
!
         CALL CHKIN  ( 'ZZEKQORD'                                )
         CALL SETMSG ( 'Invalid string bounds #:# for column #.' )
         CALL ERRINT ( '#',  CB                                  )
         CALL ERRINT ( '#',  CE                                  )
         CALL ERRINT ( '#',  N                                   )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKQORD'                                )
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

         ELSE
!
!           If the first pointer is non-zero, both pointers should have
!           been valid.
!
            CALL CHKIN  ( 'ZZEKQORD'                                   )
            CALL SETMSG ( 'Invalid string bounds #:# for the table '  // &
                          'qualifying column #.'                       )
            CALL ERRINT ( '#',  TB                                     )
            CALL ERRINT ( '#',  TE                                     )
            CALL ERRINT ( '#',  N                                      )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZEKQORD'                                   )
            RETURN

         END IF

      ELSE
!
!        No table was supplied.
!
         TABLE  =  ' '

      END IF


!
!     Set the order sense.
!
      SENSE  =  EQRYI ( BASE + EQODIR )

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
         COLIDX  =  EQRYI( BASE + EQOCOL - 1 + EQCIDX )

      ELSE

         TABIDX  =  0
         COLIDX  =  0

      END IF

      RETURN
      END
