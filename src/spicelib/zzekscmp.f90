!$Procedure      ZZEKSCMP ( EK, scalar value comparison )

      logical function zzekscmp ( op, &
                                  handle, segdsc, coldsc, row,   eltidx, &
                                  dtype,  cval,   dval,   ival,  null  )

! $ Abstract
!
!     Compare a specified scalar EK column entry with a scalar value.
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

      implicit none

      include 'ekcoldsc.inc'
      include 'ekopcd.inc'
      include 'ekqlimit.inc'
      include 'eksegdsc.inc'
      include 'ektype.inc'
      include 'ekwild.inc'

      integer               op
      integer               handle
      integer               segdsc ( sdscsz )
      integer               coldsc ( cdscsz )
      integer               row
      integer               eltidx
      integer               dtype
      character*(*)         cval
      double precision      dval
      integer               ival
      logical               null


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
      logical               failed
      logical               matchi

!
!     Local variables
!
      character*(maxstr)    eltc

      double precision      eltd
      double precision      numval

      integer               cmplen
      integer               cvlen
      integer               coltyp
      integer               elti
      integer               rel
      integer               strlen

      logical               enull
      logical               found


!
!     Use discovery check-in for speed.
!

!
!     The function value defaults to .FALSE.
!
      zzekscmp = .false.

!
!     Look up the specified column element.
!
      coltyp   =  coldsc(typidx)

      if ( coltyp .eq. chr ) then
!
!        We'll use at most the first MAXSTR characters of the input
!        string.
!
         cvlen = min ( len(cval), maxstr )

!
!        Fetch the column entry to be compared. Note that ROW
!        is a polymorphic identifier. See ZZEKRSC for details
!        on how ROW is used.
!
         call zzekrsc (  handle,  segdsc,  coldsc,  row, &
                         eltidx,  strlen,  eltc,    enull, found )

         if ( failed() ) then
!
!           Don't check out here because we haven't checked in.
!
            return
         end if

!
!        Let CMPLEN be the string length to use in comparisons.
!
         if (  found  .and.  ( .not. enull )  ) then

            cmplen = min ( strlen, maxstr )
         else
            cmplen = 0
         end if

      else if ( ( coltyp .eq. dp ) .or. ( coltyp .eq. time )  ) then

         call zzekrsd (  handle,  segdsc,  coldsc, row, &
                         eltidx,  eltd,    enull,  found )


      else if ( coltyp .eq. int ) then

         call zzekrsi (  handle,  segdsc,  coldsc, row, &
                         eltidx,  elti,    enull,  found )


      else

         call chkin  ( 'ZZEKSCMP'                         )
         call setmsg ( 'Data type code # not recognized.' )
         call errint ( '#',  coltyp                       )
         call sigerr ( 'SPICE(INVALIDDATATYPE)'           )
         call chkout ( 'ZZEKSCMP'                         )
         return

      end if


      if ( .not. found ) then

         call chkin  ( 'ZZEKSCMP'                                  )
         call setmsg ( 'EK = #; COLIDX = #; ROW = #; ELTIDX = #. '// &
                       'Column entry element was not found.'       )
         call errhan ( '#',  handle                                )
         call errint ( '#',  coldsc(ordidx)                        )
         call errint ( '#',  row                                   )
         call errint ( '#',  eltidx                                )
         call sigerr ( 'SPICE(INVALIDINDEX)'                       )
         call chkout ( 'ZZEKSCMP'                                  )
         return

      end if

!
!     Handle the ISNULL and NOTNUL operators, if perchance we see them.
!
      if ( op .eq. isnull ) then

         zzekscmp  =  enull
         return

      else if ( op .eq. notnul ) then

         zzekscmp  =  .not. enull
         return

      end if

!
!     Find the order relation that applies to the input values.
!
!     Null values precede all others.
!
      if ( enull )  then

         if ( null ) then
            rel  =  eq
         else
            rel  =  lt
         end if


      else if ( null ) then

         if ( enull ) then
            rel  =  eq
         else
            rel  =  gt
         end if


      else
!
!
!        Compare the value we looked up with the input scalar value.
!
         if (  coltyp .eq. chr ) then

            if ( dtype .ne. chr ) then

               call chkin  ( 'ZZEKSCMP'                          )
               call setmsg ( 'Column type is #; value type is #.')
               call errint ( '#', coltyp                         )
               call errint ( '#', dtype                          )
               call sigerr ( 'SPICE(BUG)'                        )
               call chkout ( 'ZZEKSCMP'                          )
               return

            end if

            if (  llt ( eltc(:cmplen), cval(:cvlen) )  )then
               rel = lt
            else if (  lgt ( eltc(:cmplen), cval(:cvlen) )  ) then
               rel = gt
            else
               rel = eq
            end if


         else if ( coltyp .eq. time ) then

            if (  ( dtype .ne. time ) .and. ( dtype .ne. dp )  ) then

               call chkin  ( 'ZZEKSCMP'                          )
               call setmsg ( 'Column type is #; value type is #.')
               call errint ( '#', coltyp                         )
               call errint ( '#', dtype                          )
               call sigerr ( 'SPICE(BUG)'                        )
               call chkout ( 'ZZEKSCMP'                          )
               return

            end if

            if ( eltd .lt. dval ) then
               rel = lt
            else if ( eltd .gt. dval ) then
               rel = gt
            else
               rel = eq
            end if


         else if ( coltyp .eq. dp ) then

            if ( dtype .eq. int ) then

               numval = ival


            else if ( ( dtype .eq. dp ) .or. ( dtype .eq. time ) ) then

               numval = dval

            else

               call chkin  ( 'ZZEKSCMP'                          )
               call setmsg ( 'Column type is #; value type is #.')
               call errint ( '#', coltyp                         )
               call errint ( '#', dtype                          )
               call sigerr ( 'SPICE(BUG)'                        )
               call chkout ( 'ZZEKSCMP'                          )
               return

            end if

            if ( eltd .lt. numval ) then
               rel = lt
            else if ( eltd .gt. numval ) then
               rel = gt
            else
               rel = eq
            end if


         else if ( coltyp .eq. int ) then

            if ( dtype .eq. int ) then

               numval = ival


            else if ( dtype .eq. dp ) then

               numval = dval

            else

               call chkin  ( 'ZZEKSCMP'                          )
               call setmsg ( 'Column type is #; value type is #.')
               call errint ( '#', coltyp                         )
               call errint ( '#', dtype                          )
               call sigerr ( 'SPICE(BUG)'                        )
               call chkout ( 'ZZEKSCMP'                          )
               return

            end if

            if ( elti .lt. numval ) then
               rel = lt
            else if ( elti .gt. numval ) then
               rel = gt
            else
               rel = eq
            end if


         else
!
!           Something untoward has happened in our column descriptor
!           argument.
!
            call chkin  ( 'ZZEKSCMP'                                )
            call setmsg ( 'The data type code # was not recognized.')
            call errint ( '#',  coltyp                              )
            call sigerr ( 'SPICE(INVALIDDATATYPE)'                  )
            call chkout ( 'ZZEKSCMP'                                )
            return

         end if

      end if

!
!     Determine the truth of the input relational expression.
!
      if ( op .eq. eq )   then

         zzekscmp  =  rel .eq. eq


      else if ( op .eq. lt )   then

         zzekscmp  =  rel .eq. lt


      else if ( op .eq. le )   then

         zzekscmp  =  rel .ne. gt


      else if ( op .eq. gt )   then

         zzekscmp  =  rel .eq. gt


      else if ( op .eq. ge )   then

         zzekscmp  =  rel .ne. lt


      else if ( op .eq. ne )   then

         zzekscmp  =  rel .ne. eq


      else if (  ( op .eq. like ) .and. ( dtype .eq. chr )  ) then

         if ( null .or. enull ) then
            zzekscmp = .false.
         else
            zzekscmp = matchi( eltc(:cmplen), cval(:cvlen), wstr, wchr )
         end if


      else if (  ( op .eq. unlike ) .and. ( dtype .eq. chr )  ) then

         if ( null .or. enull ) then
            zzekscmp = .false.
         else
            zzekscmp = .not. &
                       matchi( eltc(:cmplen), cval(:cvlen), wstr, wchr )
         end if

      else
!
!        Sorry, we couldn't resist.
!
         call chkin  ( 'ZZEKSCMP'                                      )
         call setmsg ( 'The relational operator # was not recognized '// &
                       'or was not applicable for data type #.'        )
         call errint ( '#',  op                                        )
         call errint ( '#',  dtype                                     )
         call sigerr ( 'SPICE(UNNATURALRELATION)'                      )
         call chkout ( 'ZZEKSCMP'                                      )
         return

      end if

      return
      end
