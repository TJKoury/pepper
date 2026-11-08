!$Procedure  ZZEKNRES ( Private: EK, resolve names in encoded query )

      SUBROUTINE ZZEKNRES ( QUERY, EQRYI, EQRYC, ERROR, ERRMSG, ERRPTR )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Resolve and semantically check table names, aliases, and column
!     names in an encoded EK query.
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

      INCLUDE 'ekbool.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektnamsz.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         QUERY
      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
      INTEGER               ERRPTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI     I-O  Integer component of query.
!     EQRYC     I-O  Character component of query.
!     ERROR      O   Error flag.
!     ERRMSG     O   Error message.
!     ERRPTR     O   Position in query where error was detected.
!
! $ Detailed_Input
!
!     QUERY          is the original query from which EQRYI and EQRYC
!                    were obtained.  QUERY is used only for
!                    construction of error messages.
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    On output, all names have been resolved, and
!                    table names, aliases, and column names have
!                    been semantically checked.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     ERROR          is a logical flag indicating whether an error was
!                    detected.  The error could be a name resolution
!                    error or a semantic error.
!
!     ERRMSG         is an error message describing an error in the
!                    input query, if one was detected.  If ERROR is
!                    returned .FALSE., then ERRPTR is undefined.
!
!     ERRPTR         is the character position in the original query
!                    at which an error was detected, if an error was
!                    found.  This index refers to the offending lexeme's
!                    position in the original query represented by the
!                    input encoded query.  If ERROR is returned .FALSE.,
!                    ERRPTR is undefined.
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
!         SPICE(QUERYNOTPARSED) will be signaled.  The outputs
!         will not be modified.
!
!     3)  If any sort of name resolution error or semantic error is
!         detected in the input query, the output flag ERROR is set,
!         and an error message is returned.  The checks performed by
!         this routine are listed below:
!
!           - All tables named in the FROM clause must be loaded
!             in the EK system.
!
!           - All aliases in the FROM clause must be distinct.
!
!           - No alias may be the name of a table in the FROM clause,
!             unless it is identical to the name of the table it is
!             associated with.
!
!           - No column name may be qualified with a name that is not
!             the name or alias of a table in the FROM clause.
!
!           - Each qualified column must be present in the table
!             indicated by its qualifying name.
!
!           - Each unqualified column name must be the name of a
!             column present in exactly one of the tables listed in the
!             FROM clause.
! $ Files
!
!     None.
!
! $ Particulars
!
!     Resolution of table names involves finding each table's ordinal
!     position in the FROM clause, and setting the table's descriptor
!     to record that position.  The same is done for column descriptors.
!
! $ Examples
!
!     See EKFIND.
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
      INTEGER               ISRCHC

      LOGICAL               FAILED

!
!     Local parameters
!
      INTEGER               SHORT
      PARAMETER           ( SHORT  = 32 )

!
!     Local variables
!
      CHARACTER*(TNAMSZ)    ALIAS  ( MAXTAB )
      CHARACTER*(TNAMSZ)    LTABLE
      CHARACTER*(TNAMSZ)    TABLE  ( MAXTAB )

      INTEGER               BASE
      INTEGER               CC     ( MAXTAB )
      INTEGER               CNSTYP
      INTEGER               I
      INTEGER               IPARSE
      INTEGER               J
      INTEGER               LXB
      INTEGER               LXE
      INTEGER               NCNJ
      INTEGER               NCNS
      INTEGER               NLOAD
      INTEGER               NORD
      INTEGER               NSEL
      INTEGER               NTAB

      LOGICAL               FND

!
!     No error to start with.
!
      ERROR  =  .FALSE.
      ERRMSG =  ' '
      ERRPTR =  0

!
!     The query must have been parsed at this point, or it's no go.
!
      CALL ZZEKREQI ( EQRYI, 'PARSED', IPARSE )

      IF ( FAILED() ) THEN
         RETURN
      END IF

      IF ( IPARSE .EQ. IFALSE ) THEN

         CALL CHKIN  ( 'ZZEKNRES'                            )
         CALL SETMSG ( 'Encoded query has not been parsed.'  )
         CALL SIGERR ( 'SPICE(QUERYNOTPARSED)'               )
         CALL CHKOUT ( 'ZZEKNRES'                            )
         RETURN

      END IF

!
!     Get the important counts from the query.
!
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',       NTAB )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONSTRAINTS',  NCNS )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONJUNCTIONS', NCNJ )
      CALL ZZEKREQI ( EQRYI, 'NUM_ORDERBY_COLS', NORD )
      CALL ZZEKREQI ( EQRYI, 'NUM_SELECT_COLS',  NSEL )

!
!     Start out by fetching the table names and their aliases.
!
      DO I = 1, NTAB
         CALL ZZEKQTAB ( EQRYI, EQRYC, I, TABLE(I), ALIAS(I) )
      END DO

!
!     Make sure that the aliases are distinct.  Rather than sorting
!     them, we'll check them in left-to-right order.
!
      DO I = 1, NTAB - 1

         DO J = I+1, NTAB

            IF (      (  ALIAS(I) .EQ. ALIAS(J)  ) &
                .AND. (  ALIAS(I) .NE. ' '       )  ) THEN

               ERROR  =  .TRUE.
               ERRMSG =  'Non-distinct alias <#> was found.'

               BASE   =  EQVBAS  +  ( (J-1)*2 + 1 ) * EQVDSZ
               LXB    =  EQRYI ( BASE + EQBLEX )
               LXE    =  EQRYI ( BASE + EQELEX )

               CALL REPMC ( ERRMSG, '#', QUERY(LXB:LXE), ERRMSG )

               ERRPTR = LXB

               RETURN

            END IF
!
!           We've checked the Jth alias for a match.
!
         END DO

      END DO

!
!     Make sure that no alias matches a table name other than that of
!     the table it corresponds to.
!
      DO I = 1, NTAB

         J  =  ISRCHC ( ALIAS(I), NTAB, TABLE )

         IF ( J .NE. 0 ) THEN

            IF ( J .NE. I ) THEN

               ERROR  =  .TRUE.
               ERRMSG =  'Alias <#> conflicts with table name.'

               BASE   =  EQVBAS  +  ( (I-1)*2 + 1 ) * EQVDSZ
               LXB    =  EQRYI ( BASE + EQBLEX )
               LXE    =  EQRYI ( BASE + EQELEX )

               CALL REPMC ( ERRMSG, '#', QUERY(LXB:LXE), ERRMSG )

               ERRPTR = LXB

               RETURN

            END IF

         END IF

      END DO


!
!     Make sure that all of the tables are loaded in the EK system.
!
      CALL EKNTAB ( NLOAD )

      DO I = 1, NTAB


         FND  =  .FALSE.
         J    =   1

         DO WHILE (  ( J .LE. NLOAD ) .AND. ( .NOT. FND )  )

            CALL EKTNAM ( J, LTABLE )

            IF (  TABLE(I) .EQ. LTABLE  ) THEN
!
!              When we find a loaded table, save the column count for
!              that table.
!
               FND  =  .TRUE.
               CALL EKCCNT ( TABLE, CC(I) )
            ELSE
               J    =   J + 1
            END IF

         END DO


         IF ( .NOT. FND ) THEN

            ERROR  = .TRUE.
            ERRMSG =  'Table <#> is not currently loaded.'
!
!           In order to set the error pointer, we'll need the
!           lexeme begin value for the offending table.
!
            BASE   =  EQVBAS  +   (I-1)* 2 * EQVDSZ

            LXB    =  EQRYI ( BASE + EQBLEX )
            LXE    =  EQRYI ( BASE + EQELEX )

            CALL REPMC ( ERRMSG, '#', QUERY(LXB:LXE), ERRMSG )

            ERRPTR = LXB

            RETURN

         END IF


      END DO

!
!     At this point, the tables and aliases are deemed correct.  For
!     safety, fill in each table and alias descriptor with its
!     ordinal position.
!
      DO I = 1, NTAB

         BASE  =  EQVBAS + (I-1)*2*EQVDSZ

         EQRYI( BASE          + EQTORD ) = I
         EQRYI( BASE + EQVDSZ + EQTORD ) = I

      END DO


!
!     Check the column names used in the constraints.
!
      DO I = 1, NCNS
!
!        Calculate the base address of the constraint.
!
         BASE    =  EQVBAS + (NTAB * 2 * EQVDSZ) + (I-1)*EQCDSZ

!
!        Obtain the constraint type.
!
         CNSTYP  =  EQRYI ( BASE + EQCTYP )

!
!        Check the column and table on the LHS of the constraint.
!
         CALL ZZEKCCHK ( QUERY,  EQRYI,   EQRYC, &
                         NTAB,   TABLE,   ALIAS,  BASE+EQLTAB-1, &
                         ERROR,  ERRMSG,  ERRPTR                )

         IF ( ERROR ) THEN
            RETURN
         END IF


         IF ( CNSTYP .EQ. EQCOL ) THEN
!
!           Check the column and table on the RHS of the constraint.
!
            CALL ZZEKCCHK ( QUERY,  EQRYI,   EQRYC, &
                            NTAB,   TABLE,   ALIAS,  BASE+EQRTAB-1, &
                            ERROR,  ERRMSG,  ERRPTR                )

            IF ( ERROR ) THEN
               RETURN
            END IF

         END IF


      END DO


!
!     Do the same checks and assignments for the SELECT columns.
!
      DO I = 1, NSEL
!
!        Calculate the base address of the SELECT column descriptor.
!
         BASE   =       EQVBAS &
                    +   NTAB * 2 * EQVDSZ &
                    +   NCNJ &
                    +   NCNS     * EQCDSZ &
                    +   NORD     * EQODSZ &
                    +   (I-1)    * EQSDSZ

         CALL ZZEKCCHK ( QUERY,  EQRYI,   EQRYC, &
                         NTAB,   TABLE,   ALIAS,  BASE, &
                         ERROR,  ERRMSG,  ERRPTR        )

         IF ( ERROR ) THEN
            RETURN
         END IF

      END DO


!
!     Do the same checks and assignments for the order-by columns.
!
      DO I = 1, NORD
!
!        Calculate the base address of the order-by column descriptor.
!
         BASE   =       EQVBAS &
                    +   NTAB * 2 * EQVDSZ &
                    +   NCNJ &
                    +   NCNS     * EQCDSZ &
                    +   (I-1)    * EQODSZ

         CALL ZZEKCCHK ( QUERY,  EQRYI,   EQRYC, &
                         NTAB,   TABLE,   ALIAS,  BASE, &
                         ERROR,  ERRMSG,  ERRPTR        )

         IF ( ERROR ) THEN
            RETURN
         END IF

      END DO


!
!     Indicate completion of name resolution.
!
      CALL ZZEKWEQI ( 'NAMES_RESOLVED',  ITRUE,  EQRYI  )

      END
