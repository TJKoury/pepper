!$Procedure      ZZEKPDEC ( EK, parse column declaration )

      SUBROUTINE ZZEKPDEC ( DECL, PARDSC )

      IMPLICIT NONE

! $ Abstract
!
!     Parse a declaration of a new EK column.
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

      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ektype.inc'

      CHARACTER*(*)         DECL
      INTEGER               PARDSC ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     DECL       I   String containing column declaration.
!     PARDSC     O   Partial column descriptor.
!
! $ Detailed_Input
!
!     DECL           is a character string containing a column
!                    declaration.  Column declarations are strings that
!                    contain `keyword=value' assignments that define
!                    the attributes of the columns to which they apply.
!
!                    The attributes of a column defined by a
!                    declaration are:
!
!                       DATA TYPE
!                       <size>
!                       <is the column indexed?>
!                       <are null values allowed?>
!                       <is the column fixed-count?>
!
!                    The form of a column declaration is
!
!                      .'DATATYPE     = <type descriptor>,'           //
!                      .'[SIZE        = <size descriptor>],'          //
!                      .'[INDEXED     = <boolean>],'                  //
!                      .'[NULLS_OK    = <boolean>]'                   //
!                      .'[FIXED_COUNT = <boolean>]'
!
!                    The order of the assignments does not matter.
!
!                    Here <type descriptor> can be any of
!
!                       CHARACTER*<integer length>
!                       CHARACTER*(*)
!                       DOUBLE PRECISION
!                       INTEGER
!
!                    and the optional <size descriptor> can be either of
!
!                       <integer size>
!                       VARIABLE
!
!                    Character columns may not have both variable
!                    string length and variable size.
!
!                    The column entry size defaults to 1 if the size
!                    descriptor is omitted.
!
!                    The optional clauses using the INDEXED, NULLS_OK,
!                    and FIXED_COUNT keywords take the values
!
!                       TRUE
!                       FALSE
!
!                    on the right-hand-sides of the equal signs.
!
!                    The INDEXED clause indicates that the column is
!                    indexed.  If the clause is omitted, the column is
!                    not indexed.  Only scalar-valued columns can be
!                    indexed.
!
!                    The NULLS_OK indicates that null values are
!                    permitted in the column; if the clause is omitted,
!                    null values are not permitted in the column.
!
!                    The FIXED_COUNT clause indicates that the column
!                    has a fixed number of entries; no records may be
!                    added to or deleted from the column.  If any
!                    column in a segment has a fixed record count, all
!                    columns in the segment must have the FIXED_COUNT
!                    attribute.
!
!                    FIXED_COUNT columns may be loaded only by the
!                    fast load routines.
!
!                    Unless the FIXED_COUNT keyword is used, the column
!                    does not have a fixed record count.
!
!                    Commas are required to separate the assignments
!                    within declarations.  White space is optional.
!                    Case is not significant.
!
! $ Detailed_Output
!
!     PARDSC         is an integer array that specifies the attributes
!                    of the column.  PARDSC is basically a
!                    partially-filled-in column descriptor:  it
!                    doesn't contain any pointer information.  In the
!                    locations where a column descriptor would contain
!                    an index pointer or a null flag array pointer,
!                    PARDSC contains boolean values indicating whether
!                    these items are supposed to be filled in later.
!
!                    The elements of PARDSC that are filled in upon
!                    return from this routine are:
!
!                       -- Class.  The column class is automatically
!                          determined from the declared attributes.
!
!                       -- Data type.
!
!                       -- String length, if applicable.  Variable-
!                          length strings are represented by a length
!                          specification of IFALSE.
!
!                       -- Column entry size.  Variable-size entries
!                          are represented by a size specification of
!                          IFALSE.
!
!                       -- The column's index type.  This element,
!                          which in a normal column descriptor contains
!                          an index type code, takes the boolean value
!                          ITRUE if the column is indexed and IFALSE
!                          otherwise.
!
!                       -- The column's null flag.  This element takes
!                          the boolean value ITRUE if the column can
!                          contain null values and is set to IFALSE
!                          otherwise.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input declaration does not conform to the specification
!         given in $Detailed_Input, the error SPICE(BADCOLUMNDECL) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is a utility that supports EK writing subroutines.
!
! $ Examples
!
!     1)  Parse a declaration of an indexed column of 80-character
!         strings, in which null values are allowed:
!
!            CALL ZZEKPDEC ( 'DATATYPE = CHARACTER*80, '   //
!           .                'SIZE     = 1,'               //
!           .                'INDEXED  = TRUE',            //
!           .                'NULLS_OK = TRUE',
!           .                 PARDSC                       )
!
!
!        When ZZEKPDEC returns, the values of its output column
!        descriptor will be
!
!        When ZZEKPDEC returns, the value of its output argument
!        PARDSC will be
!
!           +---------------+
!           |       3       |  Class
!           +---------------+
!           |     <CHR>     |  Data type
!           +---------------+
!           |      80       |  String length
!           +---------------+
!           |       1       |  Size
!           +---------------+
!           |       0       |  Base addres of column name (not yet set)
!           +---------------+
!           |     ITRUE     |  Index type (ITRUE means col is indexed)
!           +---------------+
!           |       0       |  Index pointer
!           +---------------+
!           |     ITRUE     |  Null flag  (ITRUE means nulls are
!           +---------------+  allowed)
!           |       0       |  Ordinal position of column in segment
!           +---------------+
!           |       0       |  Metadata pointer
!           +---------------+
!           |       0       |  (Reserved)
!           +---------------+
!
!
!
!     2)  Parse a declaration of a variable-size column of 80-character
!         strings:
!
!            CALL ZZEKPDEC ( 'DATATYPE =  CHARACTER*80, '   //
!           .                'SIZE     =  VARIABLE',
!           .                 PARDSC                        )
!
!        When ZZEKPDEC returns, the value of its output argument
!        PARDSC will be
!
!           +---------------+
!           |       3       |  Class
!           +---------------+
!           |     <CHR>     |  Data type
!           +---------------+
!           |      80       |  String length
!           +---------------+
!           |    IFALSE     |  Size (IFALSE indicates variable size)
!           +---------------+
!           |       0       |  Base addres of column name (not yet set)
!           +---------------+
!           |     IFALSE    |  Index type (IFALSE means unindexed col)
!           +---------------+
!           |       0       |  Index pointer
!           +---------------+
!           |     IFALSE    |  Null flag  (IFALSE means nulls are not
!           +---------------+  allowed)
!           |       0       |  Ordinal position of column in segment
!           +---------------+
!           |       0       |  Metadata pointer
!           +---------------+
!           |       0       |  (Reserved)
!           +---------------+
!
!
! $ Restrictions
!
!     1) Currently does not diagnose extraneous keyword assignments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-SEP-2005 (NJB)
!
!        Bug fix:  several error handling logic blocks were
!        missing SIGERR calls; these have been corrected.
!
!        Bug fix:  No diagnostic was issued for a declaration
!        of a variable-size, variable-string-length column.
!        This has been corrected.
!
!-    Beta Version 1.0.0, 16-NOV-1995 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               EQSTR
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               NATTR
      PARAMETER           ( NATTR  =   5 )

      INTEGER               MAXTOK
      PARAMETER           ( MAXTOK =  20 )

      INTEGER               TOKLEN
      PARAMETER           ( TOKLEN =  32 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 320 )

      INTEGER               NRKEY
      PARAMETER           ( NRKEY  =   1 )

!
!     Parameters naming indices of keywords in the attribute list
!     ATTKEY:
!
      INTEGER               TYPPOS
      PARAMETER           ( TYPPOS =          1 )

      INTEGER               SIZPOS
      PARAMETER           ( SIZPOS = TYPPOS + 1 )

      INTEGER               IDXPOS
      PARAMETER           ( IDXPOS = SIZPOS + 1 )

      INTEGER               NFLPOS
      PARAMETER           ( NFLPOS = IDXPOS + 1 )

      INTEGER               FXCPOS
      PARAMETER           ( FXCPOS = NFLPOS + 1 )


!
!     Local variables
!
      CHARACTER*(TOKLEN)    ATTKEY ( NATTR )
      CHARACTER*(MSGLEN)    MSG
      CHARACTER*(TOKLEN)    TOKENS ( MAXTOK )

      INTEGER               ATTLOC ( CDSCSZ )
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               PTR
      INTEGER               REQKEY ( NRKEY  )
      INTEGER               TOKLOC

      LOGICAL               ATTFND ( CDSCSZ )
      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE                  ATTKEY
      SAVE                  REQKEY


!
!     Initial values
!
      DATA                  ATTKEY         / &

                            'DATATYPE', &
                            'SIZE', &
                            'INDEXED', &
                            'NULLS_OK', &
                            'FIXED_COUNT'  /


      DATA                  REQKEY         / &
                            1              /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKPDEC' )
      END IF

!
!     Start with a clean slate.
!
      CALL CLEARI ( CDSCSZ, PARDSC )

!
!     Our declaration language has been cleverly designed so that the
!     characters
!
!        ','
!        '='
!
!     act as delimiters that LPARSM can make use
!     of.  LPARSM will hand us back a token list that contains these
!     pairs of consecutive tokens:
!
!        +----------------------+
!        | CLASS                |
!        +----------------------+
!        | <integer>            |
!        +----------------------+
!
!        +----------------------+
!        | DATATYPE             |
!        +----------------------+
!        | <type>               |
!        +----------------------+
!
!        +----------------------+
!        | SIZE                 |
!        +----------------------+
!        | <size specification> |  ( 'VARIABLE' or <integer> )
!        +----------------------+
!
!        +----------------------+
!        | INDEXED              |  (fixed-size columns only, optional)
!        +----------------------+
!        | <TRUE/FALSE>         |
!        +----------------------+
!
!        +----------------------+
!        | NULLS_OK             |  (optional)
!        +----------------------+
!        | <TRUE/FALSE>         |
!        +----------------------+
!
!
!     The order of the token pairs is not necessarily as shown.
!
!
      CALL LPARSM ( DECL, ',=', MAXTOK, N, TOKENS )

!
!     Make sure the tokens are in upper case.  They are already
!     left-justified.
!
      DO I = 1, N
         CALL UCASE( TOKENS(I), TOKENS(I) )
      END DO

!
!     See which clauses are present in the declaration, and keep track
!     of the token indices of the keywords that start the clauses.
!
      DO I = 1, NATTR
         ATTFND(I) = .FALSE.
      END DO

      DO I = 1, N

         J     = 1
         FOUND = .FALSE.

         DO WHILE (  ( J .LE. NATTR ) .AND. ( .NOT. FOUND )  )

            IF ( TOKENS(I) .EQ. ATTKEY(J) ) THEN
               FOUND     = .TRUE.
               ATTFND(J) = .TRUE.
               ATTLOC(J) =  I
            ELSE
               J         =  J + 1
            END IF

         END DO

      END DO

!
!     Make sure we got the required keyword tokens we were expecting.
!
      DO I = 1, NRKEY

         IF (  .NOT. ATTFND( REQKEY(I) ) ) THEN

            CALL SETMSG ( 'Required keyword # was not found in ' // &
                          'column declaration #.'                )
            CALL ERRCH  ( '#',  ATTKEY( REQKEY(I) )              )
            CALL ERRCH  ( '#',  DECL                             )
            CALL SIGERR ( 'SPICE(BADCOLUMDECL)'                  )
            CALL CHKOUT ( 'ZZEKPDEC'                             )
            RETURN

         END IF

      END DO

!
!     If we got this far, we can start to fill in the data type
!     descriptor.  Starting at the location of the DATATYPE keyword,
!     we should see one of the following token sequences:
!
!        DATATYPE  =  DOUBLE PRECISION
!        DATATYPE  =  INTEGER
!        DATATYPE  =  TIME
!        DATATYPE  =  CHARACTER*<integer>
!        DATATYPE  =  CHARACTER*(<integer>)
!        DATATYPE  =  CHARACTER**
!        DATATYPE  =  CHARACTER*(*)
!
!     The character declarations may have white space surrounding
!     the length specifier.
!
!     Find the location where the data type token should be.
!
      TOKLOC = ATTLOC(TYPPOS) + 1

      IF ( N .LT. TOKLOC ) THEN
         CALL SETMSG (  'Column data type specification did not '     // &
                        'follow "DATATYPE" keyword in declaration #.'  )
         CALL ERRCH  ( '#', DECL                                       )
         CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                          )
         CALL CHKOUT ( 'ZZEKPDEC'                                      )
         RETURN
      END IF

      IF ( TOKENS(TOKLOC) .EQ. 'INTEGER' ) THEN

         PARDSC(TYPIDX)  =  INT
         PARDSC(LENIDX)  =  1

      ELSE IF (  EQSTR( TOKENS(TOKLOC), 'DOUBLE PRECISION' )  ) THEN

         PARDSC(TYPIDX)  =  DP
         PARDSC(LENIDX)  =  1

      ELSE IF (  EQSTR( TOKENS(TOKLOC), 'TIME' )  ) THEN

         PARDSC(TYPIDX)  =  TIME
         PARDSC(LENIDX)  =  1

      ELSE IF (  TOKENS(TOKLOC)(1:9) .EQ. 'CHARACTER'  ) THEN

         PARDSC(TYPIDX)  =  CHR
!
!        To simplify picking up the length specification, compress
!        out blanks and parentheses.  This should leave us with
!        a token of the form
!
!           CHARACTER*<integer>
!
!        or
!
!           CHARACTER**
!
!
         CALL CMPRSS ( ' ', 0, TOKENS(TOKLOC), TOKENS(TOKLOC) )
         CALL CMPRSS ( '(', 0, TOKENS(TOKLOC), TOKENS(TOKLOC) )
         CALL CMPRSS ( ')', 0, TOKENS(TOKLOC), TOKENS(TOKLOC) )


         IF ( TOKENS(TOKLOC)(10:10) .NE. '*' ) THEN

            CALL SETMSG ( 'Required asterisk missing from character ' // &
                          'column declaration:  #  in declaration:  ' // &
                          '#'                                          )
            CALL ERRCH  ( '#', TOKENS(TOKLOC)                          )
            CALL ERRCH  ( '#', DECL                                    )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN

         END IF


         IF ( TOKENS(TOKLOC)(11:11)  .EQ.  '*'  ) THEN
!
!           The string length is variable.
!
            PARDSC(LENIDX) = IFALSE

         ELSE
!
!           The portion of the token following the asterisk should be a
!           string length.
!
            MSG = ' '
            CALL NPARSI (TOKENS(TOKLOC)(11:), PARDSC(LENIDX), MSG, PTR )

            IF ( MSG .NE. ' ' ) THEN

               CALL SETMSG ( 'String length specification # didn''t ' // &
                             'parse as an integer in declaration   #'  )
               CALL ERRCH  ( '#',  TOKENS(TOKLOC)(11:)                 )
               CALL ERRCH  ( '#',  DECL                                )
               CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                    )
               CALL CHKOUT ( 'ZZEKPDEC'                                )
               RETURN

            END IF

         END IF

      ELSE
!
!        The type specification is invalid.
!
         CALL SETMSG ( 'Data type specification # is unrecognized in '// &
                       'declaration #.'                                )
         CALL ERRCH  ( '#', TOKENS(TOKLOC)                             )
         CALL ERRCH  ( '#', DECL                                       )
         CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                          )
         CALL CHKOUT ( 'ZZEKPDEC'                                      )
         RETURN

      END IF


!
!     Next, parse the size specification, if we have one.  If it's
!     valid, it's either the string 'VARIABLE' or it's an integer.
!
      IF ( ATTFND(SIZPOS) ) THEN

         TOKLOC  =  ATTLOC(SIZPOS) + 1

         IF ( N .LT. TOKLOC ) THEN

            CALL SETMSG (  'Column size specification did not follow '// &
                           '"SIZE" keyword in declaration #.'          )
            CALL ERRCH  ( '#', DECL                                    )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN

         END IF


         IF ( TOKENS(TOKLOC) .EQ. 'VARIABLE' ) THEN
!
!           Variable size entries are not allowed for CHARACTER*(*)
!           columns.
!
            IF ( PARDSC(TYPIDX) .EQ. CHR ) THEN

               IF ( PARDSC(LENIDX) .EQ. IFALSE ) THEN

                  CALL SETMSG (  'Column size specification was ' // &
                                 'VARIABLE for a CHARACTER*(*) '  // &
                                 'column in  declaration #.'      )
                  CALL ERRCH  ( '#', DECL                         )
                  CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'            )
                  CALL CHKOUT ( 'ZZEKPDEC'                        )
                  RETURN

               END IF

            END IF


            PARDSC(SIZIDX) = IFALSE

         ELSE

            CALL NPARSI ( TOKENS(TOKLOC), PARDSC(SIZIDX), MSG,  PTR )

            IF ( MSG .NE. ' ' ) THEN

               CALL SETMSG ( 'Column element size  specification # '  // &
                             'didn''t parse as an integer in '        // &
                             'in declaration #'                        )
               CALL ERRCH  ( '#',  TOKENS(TOKLOC)                      )
               CALL ERRCH  ( '#',  DECL                                )
               CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                    )
               CALL CHKOUT ( 'ZZEKPDEC'                                )
               RETURN

            END IF

         END IF

      ELSE
!
!        If the size is not specified, it defaults to 1.
!
         PARDSC(SIZIDX) = 1

      END IF

!
!     The data type and entry size determine the column's class.
!
      IF ( PARDSC(TYPIDX) .EQ. CHR ) THEN
!
!        The character classes are 3 for scalars, 6 for arrays.
!
         IF ( PARDSC(SIZIDX) .EQ. 1 ) THEN
            PARDSC(CLSIDX) =  3
         ELSE
            PARDSC(CLSIDX) =  6
         END IF


      ELSE IF ( PARDSC(TYPIDX) .EQ. INT ) THEN
!
!        The integer classes are 1 for scalars, 4 for arrays.
!
         IF ( PARDSC(SIZIDX) .EQ. 1 ) THEN
            PARDSC(CLSIDX) =  1
         ELSE
            PARDSC(CLSIDX) =  4
         END IF


      ELSE IF (      ( PARDSC(TYPIDX) .EQ. DP   ) &
                .OR. ( PARDSC(TYPIDX) .EQ. TIME )  ) THEN
!
!        The d.p. classes are 2 for scalars, 6 for arrays.  TIME
!        values are represented using d.p. classes as well.
!
         IF ( PARDSC(SIZIDX) .EQ. 1 ) THEN
            PARDSC(CLSIDX) =  2
         ELSE
            PARDSC(CLSIDX) =  5
         END IF

      END IF

!
!     Parse the `NULLS_OK' clause, if we have one.
!
      IF ( ATTFND(NFLPOS) ) THEN

         TOKLOC = ATTLOC(NFLPOS) + 1

         IF ( N .LT. TOKLOC ) THEN

            CALL SETMSG (  'Boolean value did not follow '            // &
                           '"NULLS_OK" keyword in declaration #.'      )
            CALL ERRCH  ( '#', DECL                                    )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN

         END IF

         IF ( TOKENS(TOKLOC) .EQ. 'TRUE' ) THEN

            PARDSC(NFLIDX)  =  ITRUE

         ELSE IF ( TOKENS(TOKLOC) .EQ. 'FALSE' ) THEN

            PARDSC(NFLIDX)  =  IFALSE

         ELSE

            CALL SETMSG ( 'Invalid token # follows NULLS_OK keyword ' // &
                          'in declaration #. '                         )
            CALL ERRCH  ( '#',  TOKENS(TOKLOC)                         )
            CALL ERRCH  ( '#',  DECL                                   )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN

         END IF


      ELSE
!
!        As a default, nulls are not allowed.
!
         PARDSC(NFLIDX) = IFALSE

      END IF



!
!
!     Parse the `INDEXED' clause, if we have one.
!
      IF ( ATTFND(IDXPOS) ) THEN

         TOKLOC = ATTLOC(IDXPOS) + 1

         IF ( N .LT. TOKLOC ) THEN

            CALL SETMSG (  'Boolean value did not follow '            // &
                           '"INDEXED" keyword in declaration #.'       )
            CALL ERRCH  ( '#', DECL                                    )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN

         END IF


         IF ( TOKENS(TOKLOC) .EQ. 'TRUE' ) THEN
!
!           If we have a fixed-size column whose size is 1, then it's
!           possible to index that column.  Otherwise, we should not
!           have an `INDEXED' clause.
!
            IF ( PARDSC(SIZIDX) .NE. 1 ) THEN

               CALL SETMSG ( 'Non-scalar columns cannot be indexed. ' // &
                             'Declaration was #.'                      )
               CALL ERRCH  ( '#',  DECL                                )
               CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                    )
               CALL CHKOUT ( 'ZZEKPDEC'                                )
               RETURN

            END IF

            PARDSC(IXTIDX)  =  ITRUE

         ELSE IF ( TOKENS(TOKLOC) .EQ. 'FALSE' ) THEN

            PARDSC(IXTIDX)  =  IFALSE

         ELSE

            CALL SETMSG ( 'Invalid token # follows INDEXED keyword '  // &
                          'in declaration #. '                         )
            CALL ERRCH  ( '#',  TOKENS(TOKLOC)                         )
            CALL ERRCH  ( '#',  DECL                                   )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN

         END IF


      ELSE
!
!        As a default, the column is not indexed.
!
         PARDSC(IXTIDX) = IFALSE

      END IF




!
!     Parse the `FIXED_COUNT' clause, if we have one.
!
      IF ( ATTFND(FXCPOS) ) THEN

         TOKLOC = ATTLOC(FXCPOS) + 1

         IF ( N .LT. TOKLOC ) THEN

            CALL SETMSG (  'Boolean value did not follow '            // &
                           '"FIXED_COUNT" keyword in declaration #.'   )
            CALL ERRCH  ( '#', DECL                                    )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN

         END IF

         IF ( TOKENS(TOKLOC) .EQ. 'TRUE' ) THEN
!
!           The column is a fixed-count column.  Only scalar columns
!           are permitted to have fixed count.  We adjust the column
!           class to indicate fixed-count columns.
!
            IF ( PARDSC(CLSIDX) .EQ. 1 ) THEN
!
!              Map scalar integers.
!
               PARDSC(CLSIDX) =  7

            ELSE IF ( PARDSC(CLSIDX) .EQ. 2 ) THEN
!
!              Map scalar d.p. numbers.
!
               PARDSC(CLSIDX) =  8

            ELSE IF ( PARDSC(CLSIDX) .EQ. 3 ) THEN
!
!              Map scalar strings.
!
               PARDSC(CLSIDX) =  9

            ELSE

               CALL SETMSG ( 'FIXED_COUNT attribute used in non-'     // &
                             'scalar column declaration #. '           )
               CALL ERRCH  ( '#',  DECL                                )
               CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                    )
               CALL CHKOUT ( 'ZZEKPDEC'                                )
               RETURN

            END IF


         ELSE IF ( TOKENS(TOKLOC) .NE. 'FALSE' ) THEN
!
!           No action is required if the FIXED_COUNT keyword is set
!           to FALSE, but no value other than FALSE or TRUE may appear
!           on the RHS.
!
            CALL SETMSG ( 'Invalid token # follows NULLS_OK keyword ' // &
                          'in declaration #. '                         )
            CALL ERRCH  ( '#',  TOKENS(TOKLOC)                         )
            CALL ERRCH  ( '#',  DECL                                   )
            CALL SIGERR ( 'SPICE(BADCOLUMNDECL)'                       )
            CALL CHKOUT ( 'ZZEKPDEC'                                   )
            RETURN

         END IF


      END IF


      CALL CHKOUT ( 'ZZEKPDEC' )
      RETURN
      END
