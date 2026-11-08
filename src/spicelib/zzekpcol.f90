!$Procedure  ZZEKPCOL ( Private: EK, parse column name )

      SUBROUTINE ZZEKPCOL ( QCOL,    EQRYI,   EQRYC,   TABLE,  ALIAS, &
                            TABIDX,  COLUMN,  COLIDX,  ERROR,  ERRMSG )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Given an encoded query, parse the name of a column appearing in
!     that query, returning full particulars concerning the column and
!     its parent table.
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


      INCLUDE 'ekattdsc.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektokn.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         QCOL
      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      CHARACTER*(*)         TABLE
      CHARACTER*(*)         ALIAS
      INTEGER               TABIDX
      CHARACTER*(*)         COLUMN
      INTEGER               COLIDX
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     QCOL       I   Column name, possibly qualified.
!     EQRYI      I   Integer component of query.
!     EQRYC      I   Character component of query.
!     TABLE      O   Name of table qualifying column.
!     ALIAS      O   Alias of table, if present.
!     TABIDX     O   Index of TABLE in FROM clause, if known.
!     COLUMN     O   Name of QCOL, unqualified.
!     COLIDX     O   Index of QCOL within its parent virtual table.
!     ERROR      O   Error flag.
!     ERRMSG     O   Parse error message.
!
! $ Detailed_Input
!
!     QCOL           is a column name from an EK query.  QCOL may be
!                    qualified by a table name, in which case it
!                    conforms to the sytax
!
!                       <identifier> . <identifier>
!
!                    or QCOL may be unqualified, in which case it
!                    is simply an <identifier> in the EK query language.
!
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
! $ Detailed_Output
!
!     TABLE          is the name of the table containing the column
!                    identified by QCOL.  If QCOL contains a table name
!                    to begin with, TABLE is that name, converted to
!                    upper case.
!
!     ALIAS          is the alias of the table containing the column
!                    identified by QCOL, if an alias for that table is
!                    present in the input query.  If QCOL contains a
!                    table alias to begin with, TABLE is that alias,
!                    converted to upper case.
!
!     TABIDX         is the ordinal position in the FROM clause of the
!                    input query of the table containing the column
!                    designated by QCOL.
!
!     COLUMN         is the name of the column designated by QCOL,
!                    converted to upper case.
!
!     COLIDX         is the ordinal position column designated by QCOL
!                    with respect to the virtual table containing that
!                    column.
!
!     ERROR          is a logical flag indicating whether QCOL was
!                    parsed correctly.  The previous list of outputs
!                    are undefined if a parse error occurred.  ERROR
!                    is returned .TRUE. if a parse error occurred,
!                    .FALSE. otherwise.
!
!     ERRMSG         is a character string describing the cause of a
!                    parse error, if such an error occurred.  Otherwise,
!                    ERRMSG is returned blank.
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
!     2)  If the input query has not been semantically checked, the
!         error SPICE(NOTSEMCHECKED) will be signaled.  The outputs
!         will not be modified.
!
!     3)  If the input QCOL does not parse as a qualified or
!         unqualified column name, the error flag and message will
!         indicate that a parse error occurred.  No error will be
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports parsing of the SELECT clause of EK
!     queries by higher-level routines.  This routine is
!     superseded by the SPICELIB routine EKPSEL.
!
! $ Examples
!
!     1)  Suppose that EQRYI and EQRYC have been obtained by
!         encoding the query
!
!            'SELECT T1.COL1, T2.COL2 FROM TABLE1 T1, TABLE2 T2'
!
!         Suppose also that the table TABLE1 contains two columns
!         named COL1 and COL2, and that the columns occur in that
!         order in the table.
!
!         Then the call
!
!            CALL ZZEKPCOL ( 'T1.COL', EQRYI,  EQRYC,  TABLE, ALIAS,
!           .                TABIDX,   COLUMN, COLIDX, ERROR, ERRMSG )
!
!         will return
!
!            TABLE  =  'TABLE1'
!            ALIAS  =  'T1'
!            TABIDX =  1
!            COLUMN =  'COL1'
!            COLIDX =  1
!            ERROR  =  .FALSE.
!            ERRMSG =  ' '
!
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
!-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               ISRCHC
      LOGICAL               FAILED

!
!     Local parameters
!
      INTEGER               LONG
      PARAMETER           ( LONG   = 160 )

!
!     Local variables
!
      CHARACTER*(TNAMSZ)    ALSLST ( MAXTAB )
      CHARACTER*(LONG)      CHRBUF
      CHARACTER*(TNAMSZ)    TABLST ( MAXTAB )
      CHARACTER*(CNAMSZ)    TMPCOL
      CHARACTER*(TNAMSZ)    TMPTAB

      DOUBLE PRECISION      NUMVLS ( 3 )

      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               CC
      INTEGER               CHBEGS ( 3 )
      INTEGER               CHENDS ( 3 )
      INTEGER               I
      INTEGER               ICHECK
      INTEGER               J
      INTEGER               LXBEGS ( 3 )
      INTEGER               LXENDS ( 3 )
      INTEGER               NMATCH
      INTEGER               NTAB
      INTEGER               NTOKEN
      INTEGER               TOKENS ( 3 )
      INTEGER               VALUES ( 3 )

      LOGICAL               FND
      LOGICAL               QUAL


!
!     Use discovery check-in.
!
      ERROR   =  .FALSE.
      ERRMSG  = ' '


      CALL ZZEKREQI ( EQRYI, 'SEM_CHECKED', ICHECK )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Make sure the encoded query is in order before proceeding.
!
      IF ( ICHECK .EQ. IFALSE ) THEN

         CALL CHKIN  ( 'ZZEKPCOL'                                     )
         CALL SETMSG ( 'Encoded query has not yet been semantically '// &
                       'checked.'                                     )
         CALL SIGERR ( 'SPICE(NOTSEMCHECKED)'                         )
         CALL CHKOUT ( 'ZZEKPCOL'                                     )
         RETURN

      END IF

!
!     Scan the input column name.  There are only two valid token
!     sequences possible:
!
!        <identifier>
!
!        <identifier> . <identifier>
!
!     ZZEKSCAN should therefore return 1 or 3 tokens.
!
      CALL ZZEKSCAN ( QCOL,   3,      0,      NTOKEN, TOKENS, &
                      LXBEGS, LXENDS, VALUES, NUMVLS, CHRBUF, &
                      CHBEGS, CHENDS, ERROR,  ERRMSG          )

      IF ( ERROR ) THEN
         RETURN
      END IF


      IF ( NTOKEN .EQ. 1 ) THEN

         IF ( TOKENS(1) .NE. TKID ) THEN

            ERROR  = .TRUE.
            ERRMSG = 'Invalid column name; name should consist of ' // &
                     'an identifier.'
            RETURN

         END IF

         CALL UCASE ( QCOL, COLUMN )
         QUAL   =  .FALSE.


      ELSE IF ( NTOKEN .EQ. 3 ) THEN

         IF ( TOKENS(1) .NE. TKID  ) THEN

            ERROR  = .TRUE.
            ERRMSG = 'Invalid table name; name should consist of ' // &
                     'an identifier.'
            RETURN

         ELSE IF ( TOKENS(2) .NE. TKDOT ) THEN

            ERROR  = .TRUE.
            ERRMSG = 'Invalid qualified column name; table name ' // &
                     'should be followed by a period.'
            RETURN

         ELSE IF ( TOKENS(3) .NE. TKID ) THEN

            ERROR  = .TRUE.
            ERRMSG = 'Invalid column name; name should consist of ' // &
                     'an identifier.'
            RETURN

         END IF


         I       =  VALUES(1)
         J       =  VALUES(3)
         TMPTAB  =  CHRBUF ( CHBEGS(I) : CHENDS(I) )
         COLUMN  =  CHRBUF ( CHBEGS(J) : CHENDS(J) )
         QUAL    = .TRUE.


      ELSE

         ERROR  = .TRUE.
         ERRMSG = 'Invalid tokens present in qualified column name. ' // &
                  'Valid syntax is <column> or <table>.<column>'
         RETURN

      END IF

!
!     At this point, COLUMN and QUAL are set.  If a qualifying table
!     or alias was supplied, that string is stored in TMPTAB.  Both
!     COLUMN and TMPTAB are in upper case.
!
!     If we got this far, we'll need to look up the table names and
!     aliases from the query.
!
      CALL ZZEKREQI ( EQRYI,  'NUM_TABLES', NTAB   )

      DO I = 1, NTAB
         CALL ZZEKQTAB ( EQRYI, EQRYC, I, TABLST(I), ALSLST(I) )
      END DO

!
!     If QCOL contains a table name, look for that name in the
!     table list, and if necessary, in the alias list.
!
      IF ( QUAL ) THEN

         TABIDX  =  ISRCHC ( TMPTAB, NTAB, TABLST )

         IF ( TABIDX .EQ. 0 ) THEN
            TABIDX = ISRCHC ( TMPTAB, NTAB, ALSLST )
         END IF

!
!        If we didn't find the table name in either list, it's just
!        plain wrong.
!
         IF ( TABIDX .EQ. 0 ) THEN
            ERROR  =  .TRUE.
            ERRMSG =  'Table name <#> does not match table or alias '// &
                      'from query.'
            CALL REPMC ( ERRMSG, '#', TMPTAB, ERRMSG )
            RETURN
         END IF

!
!        At this point, TABIDX is valid.  Locate the column within
!        the table.
!
         CALL EKCCNT ( TABLST(TABIDX), CC )

         IF ( FAILED() ) THEN
            RETURN
         END IF


         FND   =  .FALSE.
         J     =   1

         DO WHILE (  ( J .LE. CC ) .AND. ( .NOT. FND )  )

            CALL EKCII ( TABLST(TABIDX), J, TMPCOL, ATTDSC )

            IF ( TMPCOL .EQ. COLUMN ) THEN
               COLIDX  =  J
               FND     =  .TRUE.
            ELSE
               J       = J + 1
            END IF

         END DO


         IF ( .NOT. FND ) THEN

            ERROR  =  .TRUE.
            ERRMSG =  'Column name <#> does not appear in the '// &
                      'qualifying table <#>.'
            CALL REPMC ( ERRMSG, '#', COLUMN, ERRMSG )
            CALL REPMC ( ERRMSG, '#', TMPTAB, ERRMSG )
            RETURN

         END IF
!
!        At this point, TABIDX and COLIDX are set correctly.
!

      ELSE
!
!        No qualifying table name was supplied.  COLUMN had better
!        be a unique column name among the set of columns belong to
!        tables in the FROM clause of the input query.  Check the
!        columns for each table in the FROM clause, looking for
!        matches.
!
         NMATCH  =  0

         DO I  = 1, NTAB

            CALL EKCCNT ( TABLST(I), CC )

            IF ( FAILED() ) THEN
               RETURN
            END IF

            DO J = 1, CC

               CALL EKCII ( TABLST(I), J, TMPCOL, ATTDSC )

               IF (  TMPCOL .EQ. COLUMN ) THEN
                  NMATCH = NMATCH + 1
                  COLUMN = TMPCOL
                  COLIDX = J
                  TABIDX = I
               END IF

            END DO

         END DO
!
!        Check to see whether we have the unique identification we're
!        hoping for.
!
         IF ( NMATCH .EQ. 0 ) THEN

            ERROR  =  .TRUE.
            ERRMSG =  'Column name <#> does not appear in any table '// &
                      'in FROM clause of query.'
            CALL REPMC ( ERRMSG, '#', COLUMN, ERRMSG )
            RETURN

         ELSE IF ( NMATCH .GT. 1 ) THEN

            ERROR  =  .TRUE.
            ERRMSG =  'Column name <#> is ambiguous without '// &
                      'a qualifying table name.'
            CALL REPMC ( ERRMSG, '#', COLUMN, ERRMSG )
            RETURN

         END IF
!
!        At this point, COLUMN, TABIDX and COLIDX are set correctly.
!

      END IF

!
!     At this point, COLUMN, TABIDX and COLIDX are set correctly,
!     regardless of whether the input name was qualified.  Fill the rest
!     of our output variables.
!
      TABLE  =  TABLST ( TABIDX )
      ALIAS  =  ALSLST ( TABIDX )

      RETURN
      END
