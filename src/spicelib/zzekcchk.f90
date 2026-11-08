!$Procedure  ZZEKCCHK ( Private: EK, check names in encoded query )

      SUBROUTINE ZZEKCCHK (  QUERY,  EQRYI,  EQRYC,  NTAB,    TABLST, &
                             ALSLST, BASE,   ERROR,  ERRMSG,  ERRPTR  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Check and resolve a specified column name in an encoded EK query.
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
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekattdsc.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekquery.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         QUERY
      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      INTEGER               NTAB
      CHARACTER*(*)         TABLST ( * )
      CHARACTER*(*)         ALSLST ( * )
      INTEGER               BASE
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
      INTEGER               ERRPTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     EQRYI     I-O  Integer component of query.
!     EQRYC     I-O  Character component of query.
!     NTAB       I   Number of tables in FROM clause.
!     TABLST     I   List of table names.
!     ALSLST     I   List of table aliases.
!     BASE       I   Base address of table/column descriptor pair.
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
!     NTAB           is the number of tables in the FROM clause of
!                    the input query.
!
!     TABLST         is a list of table names present in the FROM
!                    clause of the input query.
!
!     ALSLST         is a list of table aliases present in the FROM
!                    clause of the input query.  Absent aliases are
!                    represented by blank strings.
!
!     BASE           is the base address of a pair of descriptors
!                    for a qualified column.  The column may appear on
!                    either side of a query constraint, or it may
!                    appear in an order-by clause.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    On output, the specified column will have been
!                    resolved and checked.  Specifically, the table
!                    descriptor will have the ordinal position of the
!                    table in the FROM clause filled in, and the
!                    index of the column within the virtual table
!                    containing it will be filled in.
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
!     This routine exists for the sole purpose of centralizing code
!     used in multiple places by the name resolver ZZEKNRES.
!
!     This routine assumes that encoded EK query architecture version
!     1 is to be used with the query to be initialized; this routine
!     will not work with any other architecture version.
!
! $ Examples
!
!     See ZZEKNRES.
!
! $ Restrictions
!
!     1) This routine relies on the internals of the EK encoded query
!        structure.
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
!-    SPICELIB Version 1.1.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    SPICELIB Version 1.1.0, 15-OCT-1996 (NJB)
!
!        Error checking of column string bounds was added.
!
!-    SPICELIB Version 1.0.0, 26-SEP-1995 (NJB)
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 15-OCT-1996 (NJB)
!
!        Error checking of column string bounds was added.  These
!        bounds should never be out of range, but if they are, the
!        error diagnosis should be more graceful than a memory
!        violation.
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

!
!     Local variables
!
      CHARACTER*(CNAMSZ)    COLUMN
      CHARACTER*(1)         CTOUCH

      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               CB
      INTEGER               CC
      INTEGER               CE
      INTEGER               COLIDX
      INTEGER               I
      INTEGER               IPARSE
      INTEGER               J
      INTEGER               LXB ( 2 )
      INTEGER               NMATCH
      INTEGER               TABIDX
      INTEGER               TB
      INTEGER               TE

      LOGICAL               FND
      LOGICAL               NONAME

!
!     No error to start with.
!
      ERROR  =  .FALSE.
      ERRMSG =  ' '
      ERRPTR =  0

      CTOUCH =  QUERY(1:1)

!
!     The query must have been parsed at this point, or it's no go.
!
      CALL ZZEKREQI ( EQRYI, 'PARSED', IPARSE )

      IF ( FAILED() ) THEN
         RETURN
      END IF

      IF ( IPARSE .EQ. IFALSE ) THEN

         CALL CHKIN  ( 'ZZEKCCHK'                            )
         CALL SETMSG ( 'Encoded query has not been parsed.'  )
         CALL SIGERR ( 'SPICE(QUERYNOTPARSED)'               )
         CALL CHKOUT ( 'ZZEKCCHK'                            )
         RETURN

      END IF

!
!     Get the name and lexeme pointers for both the table and column.
!     Decide whether a table name was supplied.
!
      TB      =  EQRYI ( BASE + EQBSTR )
      TE      =  EQRYI ( BASE + EQESTR )
      LXB(1)  =  EQRYI ( BASE + EQBLEX )

      CB      =  EQRYI ( BASE + EQVDSZ + EQBSTR )
      CE      =  EQRYI ( BASE + EQVDSZ + EQESTR )
      LXB(2)  =  EQRYI ( BASE + EQVDSZ + EQBLEX )

      IF (  ( CB .LE. 0 )  .OR.  ( CE .LE. 0 )  ) THEN

         CALL CHKIN  ( 'ZZEKCCHK'                                )
         CALL SETMSG ( 'Invalid string bounds #:# for column.  ' // &
                       'Column name descriptor base is #.'       )
         CALL ERRINT ( '#',  CB                                  )
         CALL ERRINT ( '#',  CE                                  )
         CALL ERRINT ( '#',  BASE                                )
         CALL SIGERR ( 'SPICE(BUG)'                              )
         CALL CHKOUT ( 'ZZEKCCHK'                                )
         RETURN

      END IF


      NONAME  =  TB .EQ. 0


      IF ( NONAME ) THEN
!
!        If no table name is present, search for the LHS column among
!        the tables in the FROM clause.  If exactly one table
!        contains the column, that table is considered to be the
!        qualifying table.  Otherwise, the qualification is in error.
!
         NMATCH = 0

         DO I = 1, NTAB

            CALL EKCCNT ( TABLST(I), CC  )

            DO J = 1, CC

               CALL EKCII ( TABLST(I),  J,  COLUMN,  ATTDSC )

               IF (  EQRYC(CB:CE) .EQ. COLUMN ) THEN
                  NMATCH  =  NMATCH + 1
                  COLIDX  =  J
                  TABIDX  =  I
               END IF

            END DO

         END DO


         IF ( NMATCH .EQ. 0 ) THEN

            ERROR  = .TRUE.
            ERRMSG = 'Column <#> is not present in any table in FROM '// &
                     'clause.'

            CALL REPMC ( ERRMSG, '#', EQRYC(CB:CE), ERRMSG )

            ERRPTR = LXB(2)
            RETURN


         ELSE IF ( NMATCH .GT. 1 ) THEN

            ERROR  = .TRUE.
            ERRMSG = 'Column name <#> is ambiguous; a qualifying '// &
                     'table name or alias is required.'

            CALL REPMC ( ERRMSG, '#', EQRYC(CB:CE), ERRMSG )

            ERRPTR = LXB(2)
            RETURN


         END IF



      ELSE

!
!        Find the qualifying name in the FROM table list.  If the
!        name is not there, look in the alias list.
!
         TABIDX  =  ISRCHC ( EQRYC(TB:TE), NTAB, TABLST )

         IF ( TABIDX .EQ. 0 ) THEN
            TABIDX  =  ISRCHC ( EQRYC(TB:TE), NTAB, ALSLST )
         END IF

!
!        If the table name wasn't in either list, we can't use it.
!
         IF ( TABIDX .EQ. 0 ) THEN

            ERROR  = .TRUE.
            ERRMSG = 'Table name <#> is not present in FROM clause.'

            CALL REPMC ( ERRMSG, '#', EQRYC(TB:TE), ERRMSG )

            ERRPTR = LXB(1)
            RETURN

         END IF

!
!        Check the column.  This column must be present in the
!        table that qualifies it.
!
         CALL EKCCNT ( TABLST(TABIDX), CC  )


         FND  =  .FALSE.
         I    =   1

         DO WHILE (  ( I .LE. CC )  .AND. ( .NOT. FND )  )

            CALL EKCII ( TABLST(TABIDX),  I,  COLUMN,  ATTDSC )

            IF ( EQRYC(CB:CE) .EQ. COLUMN ) THEN
               FND     =  .TRUE.
               COLIDX  =  I
            ELSE
               I       =  I + 1
            END IF

         END DO


         IF ( .NOT. FND ) THEN

            ERROR  =  .TRUE.
            ERRMSG =  'Column <#> does not exist in table <#>.'

            CALL REPMC ( ERRMSG, '#', EQRYC(CB:CE), ERRMSG )
            CALL REPMC ( ERRMSG, '#', EQRYC(TB:TE), ERRMSG )

            ERRPTR = LXB(2)
            RETURN

         END IF


      END IF


!
!     If we got this far, the table and column check out.  Fill in the
!     table and column indices in their respective descriptors.
!
      EQRYI ( BASE +          EQTORD )  =  TABIDX
      EQRYI ( BASE + EQVDSZ + EQCIDX )  =  COLIDX

      RETURN
      END
