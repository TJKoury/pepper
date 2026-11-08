!$Procedure SYSELI ( Select a subset of the values of a symbol )

      subroutine syseli ( name, &
                          begin, &
                          end, &
                          tabsym, tabptr, tabval, &
                          values, &
                          found                     )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Select a subset of the values associated with a particular
!     symbol in an integer symbol table.
!
! $ Required_Reading
!
!     SYMBOLS
!
! $ Keywords
!
!     SYMBOLS
!
! $ Declarations

      implicit none

      integer(ip), parameter :: lbcell = -5

      character(*)          name
      integer(ip)           begin
      integer(ip)           end
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      integer(ip)           tabval     ( lbcell:* )
      integer(ip)           values     (        * )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose associated values are to
!                    be returned.
!     BEGIN      I   Index of the first associated value to be returned.
!     END        I   Index of the last associated value to be returned.
!     TABSYM,
!     TABPTR,
!     TABVAL     I   Components of the symbol table.
!     VALUES     O   Subset of the values associated with the symbol
!                    NAME.
!     FOUND      O   .TRUE. if the subset of values exists.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose subset of associated
!              values to be returned. If NAME is not in the symbol
!              table, FOUND is .FALSE.
!
!     BEGIN    is the index of the first associated value to be
!              returned. If BEGIN is out of range (BEGIN < 1 or
!              BEGIN > END) FOUND is .FALSE.
!
!     END      is the index of the last associated value to be
!              returned. If END is out of range (END < 1 or
!              END > is greater than the dimension of NAME)
!              FOUND is .FALSE.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the integer symbol table.
!
! $ Detailed_Output
!
!     VALUES   is a subset of the values associated with the
!              symbol NAME. If the subset specified by BEGIN and
!              END exists, as many values as will fit in VALUES
!              are returned. If the subset does not exist, no
!              values are returned and FOUND is .FALSE.
!
!     FOUND    is .TRUE. if the subset of values is exists.
!              FOUND is .FALSE. if BEGIN < 1, BEGIN > END, END < 1,
!              END > the dimension of NAME, or NAME is not
!              in the symbol table.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If there is an issue while reading the components of a
!         integer symbol table, an error is signaled by a routine in
!         the call tree of this routine. This normally indicates that
!         the integer symbol table is corrupted.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     FOUND will be false if the bounds of the subset specified by
!     BEGIN and END are out of range. Values of BEGIN and END which
!     specify bounds out of range are BEGIN < 1, BEGIN > END,
!     END < 1, or END > the dimension of NAME. FOUND is also false
!     if the symbol NAME is not in the symbol table.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        books   -->   5
!                     10
!        erasers -->   3
!        pencils -->  12
!                     18
!                     24
!                     30
!        pens    -->  10
!                     20
!                     30
!
!     Let the dimension of the array VALUES be 4.
!
!     The ouput values of VALUES and FOUND for the input values of
!     NAME, BEGIN, and END are contained in this table:
!
!        NAME         BEGIN    END     VALUES    FOUND
!        ----------   -----    ---    --------  -------
!        pencils        2       3        18      .TRUE.
!                                        24
!
!        pencils        1       4        12      .TRUE.
!                                        18
!                                        24
!                                        30
!
!        desks          1       5               .FALSE.
!
!        books          2       1               .FALSE.
!
!        erasers        1      -2               .FALSE.
!
!        pens           1       5               .FALSE.
!        ----------------------------------------------
!
!     Note that FOUND is .FALSE. for examples 3 through 6 because:
!
!     -  In the 3rd example, the symbol 'desks' is not in the
!        symbol table.
!
!     -  In the 4th example, BEGIN > END.
!
!     -  In the 5th example, END < 0.
!
!     -  In the 6th example, END is greater than the dimension of the
!        symbol 'pens'.
!
! $ Restrictions
!
!     1)  This subroutine does not check to see if the output array
!         VALUES is large enough to hold the selected set of values.
!         The caller must provide the required space.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved entry
!        from $Exceptions to $Restrictions and added entry #1 in
!        $Exceptions.
!
!-    SPICELIB Version 1.0.2, 03-NOV-2005 (NJB)
!
!        Various header corrections were made. In particular,
!        the header no longer asserts that this routine will
!        "return as many values as will fit" in the output array
!        VALUES.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     select a subset of the values of a symbol
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           bsrchc
      integer(ip)           cardc
      logical               return
      integer(ip)           sumai

!
!     Local variables
!
      integer(ip)           nsym

      integer(ip)           locsym
      integer(ip)           locval
      integer(ip)           n



!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     How many symbols to start with?
!
      nsym = cardc ( tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

      else

!
!        We could still have a problem: do these components exist?
!        Does this request even make sense?
!
         n = tabptr(locsym)

         if (       begin >= 1 &
              .and. begin <= n &
              .and. end   >= 1 &
              .and. end   <= n &
              .and. begin <= end ) then

            found  = .true.
            locval = sumai ( tabptr(1), locsym-1 ) + 1

            call movei ( tabval(locval+begin-1), end-begin+1, values )

         else
            found = .false.
         end if

      end if


      return
      end
