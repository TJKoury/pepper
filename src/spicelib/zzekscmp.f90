!$Procedure      ZZEKSCMP ( EK, scalar value comparison )

      LOGICAL FUNCTION ZZEKSCMP ( OP, &
                                  HANDLE, SEGDSC, COLDSC, ROW,   ELTIDX, &
                                  DTYPE,  CVAL,   DVAL,   IVAL,  NULL  )

! $ Abstract
!
!     Compare a specified scalar EK column entry with a scalar value.
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
!     PRIVATE
!     EK
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekopcd.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'ekwild.inc'

      INTEGER               OP
      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               COLDSC ( CDSCSZ )
      INTEGER               ROW
      INTEGER               ELTIDX
      INTEGER               DTYPE
      CHARACTER*(*)         CVAL
      DOUBLE PRECISION      DVAL
      INTEGER               IVAL
      LOGICAL               NULL


! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     OP         I   Relational operator code.
!     HANDLE     I   EK file handle.
!     SEGDSC     I   Segment descriptor.
!     COLDSC     I   Column descriptor.
!     ROW        I   ID of row containing column entry to compare.
!     ELTIDX     I   Index of element in array-valued column entry.
!     DTYPE      I   Data type of input value.
!     CVAL       I   Character string to compare with column entry.
!     DVAL       I   D.p. value to compare with column entry.
!     IVAL       I   Integer value to compare with column entry.
!     NULL       I   Flag indicating whether scalar is null.
!
!     The function returns .TRUE. if and only if the specified column
!     entry and input value of the corresponding data type satisfy the
!     relation specified by the input argument OP.
!
! $ Detailed_Input
!
!     OP             is an integer code representing a binary relational
!                    operator.  The possible values of OP are the
!                    parameters
!
!                       EQ
!                       GE
!                       GT
!                       LE
!                       LIKE
!                       LT
!                       NE
!                       ISNULL
!                       NOTNUL
!
!
!     HANDLE         is an EK file handle.  The file may be open for
!                    reading or writing.
!
!     SEGDSC         is the EK segment descriptor of the column entry
!                    to be compared.
!
!     COLDSC         is an EK column descriptor for the column
!                    containing the entry to be compared.
!
!     ROW            is the identifier of the row containing the column
!                    entry to be compared. Note that these identifiers
!                    are polymorphic: their meaning is a function of
!                    the class of column that contains the entry of
!                    interest.
!
!     ELTIDX         is the index of the column entry element to be
!                    compared, if the column is array-valued.  ELTIDX
!                    is ignored for scalar columns.
!
!     DTYPE          is the data type of the input scalar value.
!
!
!     CVAL,
!     DVAL,
!     IVAL           are, respectively, character, double precision,
!                    and integer scalar variables.  The column entry
!                    is compared against whichever of these has the
!                    same data type as the entry; the other two
!                    variables are ignored.  If the data type of the
!                    column entry is TIME, the entry is compared with
!                    the variable DVAL.
!
!     NULL
!
! $ Detailed_Output
!
!     The function returns .TRUE. if and only if the specified column
!     entry and input value of the corresponding data type satisfy the
!     relation specified by the input argument OP.
!
!     If the specified column entry is null, it is considered to
!     precede all non-null values, and the logical value of the
!     expression
!
!        <column element> OP <value>
!
!     is determined accordingly.  Null character values do not satisfy
!     the relation
!
!        <null column element> LIKE <character value>
!
!     for any character value.
!
! $ Parameters
!
!     Within the EK system, relational operators used in EK queries are
!     represented by integer codes.  The codes and their meanings are
!     listed below.
!
!     Relational expressions in EK queries have the form
!
!        <column name> <operator> <value>
!
!     For columns containing numeric values, the operators
!
!        EQ,  GE,  GT,  LE,  LT,  NE
!
!     may be used; these operators have the same meanings as their
!     Fortran counterparts.  For columns containing character values,
!     the list of allowed operators includes those in the above list,
!     and in addition includes the operator
!
!        LIKE
!
!     which is used to compare strings to a template.  In the character
!     case, the meanings of the parameters
!
!        GE,  GT,  LE,  LT
!
!     match those of the Fortran lexical functions
!
!        LGE, LGT, LLE, LLT
!
!     Null values are considered to precede all non-null values.
!
! $ Exceptions
!
!     1)  If the input file handle is invalid, the error will be
!         diagnosed by routines called by this routine.
!         The function value is .FALSE. in this case.
!
!     2)  If an I/O error occurs while attempting to find the address
!         range of the specified column entry element, the error will
!         be diagnosed by routines called by this routine.  The
!         function value is .FALSE. in this case.
!
!     3)  If any of SEGDSC, COLDSC, or ROW are invalid, this routine
!         may fail in unpredictable, but possibly spectacular, ways.
!         Except as described in this header section, no attempt is
!         made to handle these errors.
!
!     4)  If the data type code in the input column descriptor is not
!         recognized, the error SPICE(INVALIDDATATYPE) is signaled.
!         The function value is .FALSE. in this case.
!
!     5)  If the specified column entry cannot be found, the error
!         SPICE(INVALIDINDEX) is signaled.  The function value is
!         .FALSE. in this case.
!
!     6)  If the relational operator code OP is not recognized, the
!         error SPICE(UNNATURALRELATION) is signaled.  The function
!         value is .FALSE. in this case.
!
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     This routine is an EK utility intended to centralize a frequently
!     performed comparison operation.
!
! $ Examples
!
!     See ZZEKRMCH.
!
! $ Restrictions
!
!     1)  This routine must execute quickly.  Therefore, it checks in
!         only if it detects an error.  If an error is signaled by a
!         routine called by this routine, this routine will not appear
!         in the SPICELIB traceback display.  Also, in the interest
!         of speed, this routine does not test the value of the SPICELIB
!         function RETURN upon entry.
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
!-    SPICELIB Version 1.3.0, 09-FEB-2015 (NJB)
!
!        Now uses ERRHAN to insert DAS file name into
!        long error messages.
!
!-    SPICELIB Version 1.2.0, 31-MAY-2009 (NJB)
!
!        Bug fix: routine failed to account for the possibility
!        that scalar string column entries can have unlimited
!        length. Now at most the first MAXSTR characters of such
!        an entry are used in comparisons.
!
!-    SPICELIB Version 1.1.0, 21-DEC-2001 (NJB)
!
!        Bug fix:  routine now indicates "no match" when operator
!        is LIKE or UNLIKE and column entry is null.
!
!-    SPICELIB Version 1.0.0, 17-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               MATCHI

!
!     Local variables
!
      CHARACTER*(MAXSTR)    ELTC

      DOUBLE PRECISION      ELTD
      DOUBLE PRECISION      NUMVAL

      INTEGER               CMPLEN
      INTEGER               CVLEN
      INTEGER               COLTYP
      INTEGER               ELTI
      INTEGER               REL
      INTEGER               STRLEN

      LOGICAL               ENULL
      LOGICAL               FOUND


!
!     Use discovery check-in for speed.
!

!
!     The function value defaults to .FALSE.
!
      ZZEKSCMP = .FALSE.

!
!     Look up the specified column element.
!
      COLTYP   =  COLDSC(TYPIDX)

      IF ( COLTYP .EQ. CHR ) THEN
!
!        We'll use at most the first MAXSTR characters of the input
!        string.
!
         CVLEN = MIN ( LEN(CVAL), MAXSTR )

!
!        Fetch the column entry to be compared. Note that ROW
!        is a polymorphic identifier. See ZZEKRSC for details
!        on how ROW is used.
!
         CALL ZZEKRSC (  HANDLE,  SEGDSC,  COLDSC,  ROW, &
                         ELTIDX,  STRLEN,  ELTC,    ENULL, FOUND )

         IF ( FAILED() ) THEN
!
!           Don't check out here because we haven't checked in.
!
            RETURN
         END IF

!
!        Let CMPLEN be the string length to use in comparisons.
!
         IF (  FOUND  .AND.  ( .NOT. ENULL )  ) THEN

            CMPLEN = MIN ( STRLEN, MAXSTR )
         ELSE
            CMPLEN = 0
         END IF

      ELSE IF ( ( COLTYP .EQ. DP ) .OR. ( COLTYP .EQ. TIME )  ) THEN

         CALL ZZEKRSD (  HANDLE,  SEGDSC,  COLDSC, ROW, &
                         ELTIDX,  ELTD,    ENULL,  FOUND )


      ELSE IF ( COLTYP .EQ. INT ) THEN

         CALL ZZEKRSI (  HANDLE,  SEGDSC,  COLDSC, ROW, &
                         ELTIDX,  ELTI,    ENULL,  FOUND )


      ELSE

         CALL CHKIN  ( 'ZZEKSCMP'                         )
         CALL SETMSG ( 'Data type code # not recognized.' )
         CALL ERRINT ( '#',  COLTYP                       )
         CALL SIGERR ( 'SPICE(INVALIDDATATYPE)'           )
         CALL CHKOUT ( 'ZZEKSCMP'                         )
         RETURN

      END IF


      IF ( .NOT. FOUND ) THEN

         CALL CHKIN  ( 'ZZEKSCMP'                                  )
         CALL SETMSG ( 'EK = #; COLIDX = #; ROW = #; ELTIDX = #. '// &
                       'Column entry element was not found.'       )
         CALL ERRHAN ( '#',  HANDLE                                )
         CALL ERRINT ( '#',  COLDSC(ORDIDX)                        )
         CALL ERRINT ( '#',  ROW                                   )
         CALL ERRINT ( '#',  ELTIDX                                )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)'                       )
         CALL CHKOUT ( 'ZZEKSCMP'                                  )
         RETURN

      END IF

!
!     Handle the ISNULL and NOTNUL operators, if perchance we see them.
!
      IF ( OP .EQ. ISNULL ) THEN

         ZZEKSCMP  =  ENULL
         RETURN

      ELSE IF ( OP .EQ. NOTNUL ) THEN

         ZZEKSCMP  =  .NOT. ENULL
         RETURN

      END IF

!
!     Find the order relation that applies to the input values.
!
!     Null values precede all others.
!
      IF ( ENULL )  THEN

         IF ( NULL ) THEN
            REL  =  EQ
         ELSE
            REL  =  LT
         END IF


      ELSE IF ( NULL ) THEN

         IF ( ENULL ) THEN
            REL  =  EQ
         ELSE
            REL  =  GT
         END IF


      ELSE
!
!
!        Compare the value we looked up with the input scalar value.
!
         IF (  COLTYP .EQ. CHR ) THEN

            IF ( DTYPE .NE. CHR ) THEN

               CALL CHKIN  ( 'ZZEKSCMP'                          )
               CALL SETMSG ( 'Column type is #; value type is #.')
               CALL ERRINT ( '#', COLTYP                         )
               CALL ERRINT ( '#', DTYPE                          )
               CALL SIGERR ( 'SPICE(BUG)'                        )
               CALL CHKOUT ( 'ZZEKSCMP'                          )
               RETURN

            END IF

            IF (  LLT ( ELTC(:CMPLEN), CVAL(:CVLEN) )  )THEN
               REL = LT
            ELSE IF (  LGT ( ELTC(:CMPLEN), CVAL(:CVLEN) )  ) THEN
               REL = GT
            ELSE
               REL = EQ
            END IF


         ELSE IF ( COLTYP .EQ. TIME ) THEN

            IF (  ( DTYPE .NE. TIME ) .AND. ( DTYPE .NE. DP )  ) THEN

               CALL CHKIN  ( 'ZZEKSCMP'                          )
               CALL SETMSG ( 'Column type is #; value type is #.')
               CALL ERRINT ( '#', COLTYP                         )
               CALL ERRINT ( '#', DTYPE                          )
               CALL SIGERR ( 'SPICE(BUG)'                        )
               CALL CHKOUT ( 'ZZEKSCMP'                          )
               RETURN

            END IF

            IF ( ELTD .LT. DVAL ) THEN
               REL = LT
            ELSE IF ( ELTD .GT. DVAL ) THEN
               REL = GT
            ELSE
               REL = EQ
            END IF


         ELSE IF ( COLTYP .EQ. DP ) THEN

            IF ( DTYPE .EQ. INT ) THEN

               NUMVAL = IVAL


            ELSE IF ( ( DTYPE .EQ. DP ) .OR. ( DTYPE .EQ. TIME ) ) THEN

               NUMVAL = DVAL

            ELSE

               CALL CHKIN  ( 'ZZEKSCMP'                          )
               CALL SETMSG ( 'Column type is #; value type is #.')
               CALL ERRINT ( '#', COLTYP                         )
               CALL ERRINT ( '#', DTYPE                          )
               CALL SIGERR ( 'SPICE(BUG)'                        )
               CALL CHKOUT ( 'ZZEKSCMP'                          )
               RETURN

            END IF

            IF ( ELTD .LT. NUMVAL ) THEN
               REL = LT
            ELSE IF ( ELTD .GT. NUMVAL ) THEN
               REL = GT
            ELSE
               REL = EQ
            END IF


         ELSE IF ( COLTYP .EQ. INT ) THEN

            IF ( DTYPE .EQ. INT ) THEN

               NUMVAL = IVAL


            ELSE IF ( DTYPE .EQ. DP ) THEN

               NUMVAL = DVAL

            ELSE

               CALL CHKIN  ( 'ZZEKSCMP'                          )
               CALL SETMSG ( 'Column type is #; value type is #.')
               CALL ERRINT ( '#', COLTYP                         )
               CALL ERRINT ( '#', DTYPE                          )
               CALL SIGERR ( 'SPICE(BUG)'                        )
               CALL CHKOUT ( 'ZZEKSCMP'                          )
               RETURN

            END IF

            IF ( ELTI .LT. NUMVAL ) THEN
               REL = LT
            ELSE IF ( ELTI .GT. NUMVAL ) THEN
               REL = GT
            ELSE
               REL = EQ
            END IF


         ELSE
!
!           Something untoward has happened in our column descriptor
!           argument.
!
            CALL CHKIN  ( 'ZZEKSCMP'                                )
            CALL SETMSG ( 'The data type code # was not recognized.')
            CALL ERRINT ( '#',  COLTYP                              )
            CALL SIGERR ( 'SPICE(INVALIDDATATYPE)'                  )
            CALL CHKOUT ( 'ZZEKSCMP'                                )
            RETURN

         END IF

      END IF

!
!     Determine the truth of the input relational expression.
!
      IF ( OP .EQ. EQ )   THEN

         ZZEKSCMP  =  REL .EQ. EQ


      ELSE IF ( OP .EQ. LT )   THEN

         ZZEKSCMP  =  REL .EQ. LT


      ELSE IF ( OP .EQ. LE )   THEN

         ZZEKSCMP  =  REL .NE. GT


      ELSE IF ( OP .EQ. GT )   THEN

         ZZEKSCMP  =  REL .EQ. GT


      ELSE IF ( OP .EQ. GE )   THEN

         ZZEKSCMP  =  REL .NE. LT


      ELSE IF ( OP .EQ. NE )   THEN

         ZZEKSCMP  =  REL .NE. EQ


      ELSE IF (  ( OP .EQ. LIKE ) .AND. ( DTYPE .EQ. CHR )  ) THEN

         IF ( NULL .OR. ENULL ) THEN
            ZZEKSCMP = .FALSE.
         ELSE
            ZZEKSCMP = MATCHI( ELTC(:CMPLEN), CVAL(:CVLEN), WSTR, WCHR )
         END IF


      ELSE IF (  ( OP .EQ. UNLIKE ) .AND. ( DTYPE .EQ. CHR )  ) THEN

         IF ( NULL .OR. ENULL ) THEN
            ZZEKSCMP = .FALSE.
         ELSE
            ZZEKSCMP = .NOT. &
                       MATCHI( ELTC(:CMPLEN), CVAL(:CVLEN), WSTR, WCHR )
         END IF

      ELSE
!
!        Sorry, we couldn't resist.
!
         CALL CHKIN  ( 'ZZEKSCMP'                                      )
         CALL SETMSG ( 'The relational operator # was not recognized '// &
                       'or was not applicable for data type #.'        )
         CALL ERRINT ( '#',  OP                                        )
         CALL ERRINT ( '#',  DTYPE                                     )
         CALL SIGERR ( 'SPICE(UNNATURALRELATION)'                      )
         CALL CHKOUT ( 'ZZEKSCMP'                                      )
         RETURN

      END IF

      RETURN
      END
