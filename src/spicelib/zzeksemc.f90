!$Procedure  ZZEKSEMC ( Private: EK, semantically check encoded query )

      SUBROUTINE ZZEKSEMC ( QUERY, EQRYI, EQRYC, ERROR, ERRMSG, ERRPTR )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Semantically check an encoded EK query.
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
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekquery.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'

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
!     QUERY      I   Original query.
!     EQRYI     I-O  Integer component of query.
!     EQRYC     I-O  Character component of query.
!     ERROR      O   Error flag.
!     ERRMSG     O   Semantic error message.
!     ERRPTR     O   Position in query where error was detected.
!
! $ Detailed_Input
!
!     QUERY          is the original query from which EQRYI and EQRYC
!                    were obtained.  QUERY is used only for
!                    construction of error messages.
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    The query must have been parsed and must have
!                    its table and column names resolved.  Time values
!                    must also have been resolved.
!
!     EQRYC          is the character portion of an encoded EK query.
!
! $ Detailed_Output
!
!     EQRYI          is the integer portion of an encoded EK query.
!                    On output, semantic checking will have been
!                    performed.
!
!     EQRYC          is the character portion of an encoded EK query.
!
!     ERROR          is a logical flag indicating whether a semantic
!                    error was detected.
!
!     ERRMSG         is an error message describing a semantic error,
!                    if such an error was detected.  If ERROR is
!                    returned .FALSE., then ERRPTR is undefined.
!
!     ERRPTR         is the character position in the original query
!                    at which a semantic error was detected, if the
!                    input query contains a semantic error.  This
!                    index refers to the offending lexeme's position in
!                    the original query represented by the input encoded
!                    query.  If ERROR is returned .FALSE., ERRPTR is
!                    undefined.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If no EK files are loaded at the time this routine is called,
!         the error will be diagnosed by routines called by this
!         routine.
!
!     2)  If the input query is not initialized, the error will be
!         diagnosed by routines called by this routine.  The outputs
!         will not be modified.
!
!     3)  If the input query has not had its names resolved, the error
!         SPICE(UNRESOLVEDNAMES) will be signaled.  The outputs
!         will not be modified.
!
!     4)  If the input query contains time values that have not been
!         resolved, the error SPICE(UNRESOLVEDTIMES) will be signaled.
!         The outputs will not be modified.
!
!     5)  If any sort of semantic error is detected in the input query,
!         the output flag ERROR is set, an error message is returned,
!         and LXBEG and LXEND are set to indicate the location of the
!         first lexeme at which an error was detected.
!
!         The checks performed by this routine are listed below:
!
!           - Constraints comparing values from two columns must
!             refer to columns having identical data types, or else
!             both types must be numeric.
!
!           - Constraints comparing values from a column with literal
!             values must refer to columns having the data type of the
!             literal value.
!
!           - The LIKE and NOT LIKE operators may be used only with
!             string values.
!
!           - Columns named in constraints must be scalar-valued.
!
!           - Columns named as order-by columns must be scalar-valued.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The semantic checking performed by this routine is dependent on
!     the kernels loaded at the time this routine is called.
!
!     This routine assumes that encoded EK query architecture version
!     1 is to be used with the query to be initialized; this routine
!     will not work with any other architecture version.
!
! $ Examples
!
!     See EKFIND.
!
! $ Restrictions
!
!     1) Loading or unloading EK files between name resolution of the
!        the input query and passing the query to this routine will
!        invalidate the checking done by this routine, and may cause
!        the routine to fail.
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
!-    Beta Version 1.0.0, 23-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local parameters
!
      INTEGER               SHORT
      PARAMETER           ( SHORT  = 32 )

!
!     Local variables
!

      CHARACTER*(TNAMSZ)    ALIAS
      CHARACTER*(CNAMSZ)    COLNAM
      CHARACTER*(TNAMSZ)    LHSTAB
      CHARACTER*(TNAMSZ)    ORDTAB
      CHARACTER*(TNAMSZ)    RHSTAB
      CHARACTER*(SHORT)     TYPSTR ( 4 )

      INTEGER               ATTDSC ( ADSCSZ )
      INTEGER               BASE
      INTEGER               CNSTYP
      INTEGER               COLIDX
      INTEGER               I
      INTEGER               IRSOLV
      INTEGER               LHSSIZ
      INTEGER               LHSTYP
      INTEGER               LXB    ( 2 )
      INTEGER               LXE    ( 2 )
      INTEGER               NCNJ
      INTEGER               NCNS
      INTEGER               NORD
      INTEGER               NTAB
      INTEGER               OPCODE
      INTEGER               RHSSIZ
      INTEGER               RHSTYP
      INTEGER               TABIDX
      INTEGER               TRSOLV

      LOGICAL               LIKEOP
      LOGICAL               NULVAL

!
!     Saved variables
!
      SAVE                  TYPSTR

!
!     Initial values
!
      DATA                  TYPSTR  /  'CHARACTER', &
                                       'DOUBLE PRECISION', &
                                       'INTEGER', &
                                       'TIME'               /

!
!     Use discovery check-in.
!
      ERROR   =  .FALSE.
      ERRMSG  = ' '
      ERRPTR  = 0

      CALL ZZEKREQI ( EQRYI, 'NAMES_RESOLVED', IRSOLV )

      IF ( FAILED() ) THEN
         RETURN
      END IF

      IF ( IRSOLV .EQ. IFALSE ) THEN

         CALL CHKIN  ( 'ZZEKSEMC'                                   )
         CALL SETMSG ( 'Encoded query has not had names resolved.'  )
         CALL SIGERR ( 'SPICE(UNRESOLVEDNAMES)'                     )
         CALL CHKOUT ( 'ZZEKSEMC'                                   )
         RETURN

      END IF


      CALL ZZEKREQI ( EQRYI, 'TIMES_RESOLVED', TRSOLV )

      IF ( FAILED() ) THEN
         RETURN
      END IF

      IF ( TRSOLV .EQ. IFALSE ) THEN

         CALL CHKIN  ( 'ZZEKSEMC'                                   )
         CALL SETMSG ( 'Encoded query has not had time values '     // &
                       'resolved.'                                  )
         CALL SIGERR ( 'SPICE(UNRESOLVEDTIMES)'                     )
         CALL CHKOUT ( 'ZZEKSEMC'                                   )
         RETURN

      END IF

!
!     Get the important counts from the query.
!
      CALL ZZEKREQI ( EQRYI, 'NUM_TABLES',        NTAB )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONSTRAINTS',   NCNS )
      CALL ZZEKREQI ( EQRYI, 'NUM_CONJUNCTIONS',  NCNJ )
      CALL ZZEKREQI ( EQRYI, 'NUM_ORDERBY_COLS',  NORD )

!
!     Perform semantic checks applicable to constraints.
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
!        Get the index of the table containing the LHS column, and get
!        the index of this column within that table.  Look up the
!        table name.
!
         TABIDX  =  EQRYI ( BASE + EQLTAB - 1 + EQTORD )
         COLIDX  =  EQRYI ( BASE + EQLCOL - 1 + EQCIDX )
         LXB(1)  =  EQRYI ( BASE + EQLCOL - 1 + EQBLEX )
         LXE(1)  =  EQRYI ( BASE + EQLCOL - 1 + EQELEX )

         CALL ZZEKQTAB ( EQRYI, EQRYC, TABIDX, LHSTAB, ALIAS )

!
!        Look up the name and attributes of the column on the LHS of the
!        constraint.
!
         CALL EKCII ( LHSTAB, COLIDX, COLNAM, ATTDSC )

         LHSTYP  =  ATTDSC(ATTTYP)
         LHSSIZ  =  ATTDSC(ATTSIZ)

         IF ( LHSSIZ .NE. 1 ) THEN

            ERROR  =  .TRUE.
            ERRMSG =  'Non-scalar column <#> having size # found '  // &
                      'in query constraint.'
            CALL REPMC  ( ERRMSG, '#', QUERY(LXB(1):LXE(2)), ERRMSG )
            CALL REPMI  ( ERRMSG, '#', LHSSIZ,               ERRMSG )

            ERRPTR =  LXB(1)
            RETURN

         END IF

!
!        Get the operator for the current constraint.
!
         OPCODE  =  EQRYI ( BASE + EQOPCD )

!
!        Decide whether the constraint is an `IS NULL' or `IS NOT NULL'
!        test.
!
         NULVAL  =  ( OPCODE .EQ. ISNULL ) .OR. ( OPCODE .EQ. NOTNUL )

!
!        Check for use of the LIKE or NOT LIKE operators.  These
!        operators may be used only if the LHS column has character
!        type.
!
         LIKEOP  =  ( OPCODE .EQ. LIKE ) .OR. ( OPCODE .EQ. UNLIKE )


         IF (  LIKEOP  .AND.  ( LHSTYP .NE. CHR )  ) THEN

            ERROR  =  .TRUE.
            ERRMSG = 'LIKE and NOT LIKE operators may be used only ' // &
                     'with character columns.  Column <#> has type '// &
                     '#.'

            CALL REPMC ( ERRMSG, '#', QUERY(LXB(1):LXE(1)),  ERRMSG )
            CALL REPMC ( ERRMSG, '#', TYPSTR(LHSTYP),        ERRMSG )

            ERRPTR  =  LXB(1)
            RETURN

         END IF


!
!        If the constraint compares two columns, get the same
!        information for the RHS column.
!
         IF ( CNSTYP .EQ. EQCOL ) THEN

            TABIDX  =  EQRYI ( BASE + EQRTAB - 1 + EQTORD )
            COLIDX  =  EQRYI ( BASE + EQRCOL - 1 + EQCIDX )
            LXB(2)  =  EQRYI ( BASE + EQRCOL - 1 + EQBLEX )
            LXE(2)  =  EQRYI ( BASE + EQRCOL - 1 + EQELEX )

            CALL ZZEKQTAB ( EQRYI, EQRYC, TABIDX, RHSTAB, ALIAS )

!
!           Look up the name and attributes of the column on the RHS of
!           the constraint.
!
            CALL EKCII ( RHSTAB, COLIDX, COLNAM, ATTDSC )

            RHSTYP  =  ATTDSC(2)
            RHSSIZ  =  ATTDSC(4)

            IF ( RHSSIZ .NE. 1 ) THEN

               ERROR  =  .TRUE.
               ERRMSG =  'Non-scalar column <#> having size # found '// &
                         'in query constraint.'
               CALL REPMC  ( ERRMSG, '#', QUERY(LXB(1):LXE(2)), ERRMSG )
               CALL REPMI  ( ERRMSG, '#', RHSSIZ,               ERRMSG )

               ERRPTR =  LXB(2)
               RETURN

            END IF

!
!           Check for data type mismatch.
!
            IF ( RHSTYP .NE. LHSTYP ) THEN
!
!              The only allowed mismatch is between integers and
!              d.p. numbers.
!
               IF (      ( LHSTYP .EQ. TIME ) &
                    .OR. ( LHSTYP .EQ. CHR  ) &
                    .OR. ( RHSTYP .EQ. TIME ) &
                    .OR. ( RHSTYP .EQ. CHR  )  ) THEN

                  ERROR  =  .TRUE.
                  ERRMSG = 'Data type mismatch: column <#> has data ' // &
                           'type #; column <#> has data type #.'

                  CALL REPMC (ERRMSG, '#', QUERY(LXB(1):LXE(1)), ERRMSG)
                  CALL REPMC (ERRMSG, '#', TYPSTR(LHSTYP),       ERRMSG)
                  CALL REPMC (ERRMSG, '#', QUERY(LXB(2):LXE(2)), ERRMSG)
                  CALL REPMC (ERRMSG, '#', TYPSTR(RHSTYP),       ERRMSG)

                  ERRPTR = LXB(2)
                  RETURN

               END IF

            END IF


         ELSE
!
!           The constraint compares a column against a value.  If the
!           operator is `IS NULL' or `IS NOT NULL', there are no
!           further semantic checks to be made.
!
            IF ( NULVAL ) THEN
               RETURN
            END IF

!
!           Get the data type of the value on the RHS.
!
            RHSTYP  =  EQRYI ( BASE + EQBVAL - 1 + EQDTYP )
            LXB(2)  =  EQRYI ( BASE + EQBVAL - 1 + EQBLEX )
            LXE(2)  =  EQRYI ( BASE + EQBVAL - 1 + EQELEX )

            IF ( RHSTYP .NE. LHSTYP ) THEN
!
!              The only allowed mismatch is between integers and
!              d.p. numbers.
!
               IF (       ( LHSTYP .EQ. TIME ) &
                     .OR. ( LHSTYP .EQ. CHR  ) &
                     .OR. ( RHSTYP .EQ. TIME ) &
                     .OR. ( RHSTYP .EQ. CHR  )  ) THEN

                  ERROR  =  .TRUE.
                  ERRMSG = 'Data type mismatch: column <#> has data ' // &
                           'type #; value <#> has data type #.'

                  CALL REPMC (ERRMSG, '#', QUERY(LXB(1):LXE(1)), ERRMSG)
                  CALL REPMC (ERRMSG, '#', TYPSTR(LHSTYP),       ERRMSG)
                  CALL REPMC (ERRMSG, '#', QUERY(LXB(2):LXE(2)), ERRMSG)
                  CALL REPMC (ERRMSG, '#', TYPSTR(RHSTYP),       ERRMSG)

                  ERRPTR = LXB(2)
                  RETURN

               END IF

            END IF


         END IF

!
!        We've finished the checks on the current constraint.
!
      END DO

!
!     Now check the order-by columns, if any are present.  These
!     columns must have scalar type.
!
      DO I = 1, NORD
!
!        Get the query column descriptor for the Ith order-by column.
!
         BASE   =       EQVBAS &
                    +   NTAB * 2 * EQVDSZ &
                    +   NCNJ &
                    +   NCNS     * EQCDSZ &
                    +   (I-1)    * EQODSZ
!
!        Look up the attributes of the column.  It's the size we're
!        after.
!
         TABIDX  =  EQRYI ( BASE              + EQTORD )
         COLIDX  =  EQRYI ( BASE + EQOCOL - 1 + EQCIDX )
         LXB(1)  =  EQRYI ( BASE + EQOCOL - 1 + EQBLEX )
         LXE(1)  =  EQRYI ( BASE + EQOCOL - 1 + EQELEX )

         CALL ZZEKQTAB ( EQRYI, EQRYC, TABIDX, ORDTAB, ALIAS )


         CALL EKCII ( ORDTAB, COLIDX, COLNAM, ATTDSC )


         IF ( ATTDSC(4) .NE. 1 ) THEN

            ERROR  =  .TRUE.
            ERRMSG =  'Non-scalar column <#> having size # found '  // &
                      'in order-by column.'
            CALL REPMC  ( ERRMSG, '#', QUERY(LXB(1):LXE(2)), ERRMSG )
            CALL REPMI  ( ERRMSG, '#', ATTDSC(4),            ERRMSG )

            ERRPTR =  LXB(1)
            RETURN

         END IF

      END DO

!
!     Indicate completion of semantic checking.
!
      CALL ZZEKWEQI (  'SEM_CHECKED',  ITRUE,  EQRYI  )

      RETURN
      END
