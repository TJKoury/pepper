!> The parameters of SPICELIB's include file ekqlimit.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekqlimit

   implicit none

   public

!     Include Section:  EK Query Limit Parameters
!
!        ekqlimit.inc  Version 3    16-NOV-1995 (NJB)
!
!           Parameter MAXCON increased to 1000.
!
!        ekqlimit.inc  Version 2    01-AUG-1995 (NJB)
!
!           Updated to support SELECT clause.
!
!
!        ekqlimit.inc  Version 1    07-FEB-1995 (NJB)
!
!
!     These limits apply to character string queries input to the
!     EK scanner.  This limits are part of the EK system's user
!     interface:  the values should be advertised in the EK required
!     reading document.
!
!
!     Maximum length of an input query:  MAXQRY.  This value is
!     currently set to twenty-five 80-character lines.
!
   integer               maxqry
   parameter           ( maxqry = 2000 )

!
!     Maximum number of columns that may be listed in the
!     `order-by clause' of a query:  MAXSEL.  MAXSEL = 50.
!
   integer               maxsel
   parameter           ( maxsel = 50 )

!
!     Maximum number of tables that may be listed in the `FROM
!     clause' of a query: MAXTAB.
!
   integer               maxtab
   parameter           ( maxtab = 10 )

!
!     Maximum number of relational expressions that may be listed
!     in the `constraint clause' of a query: MAXCON.
!
!     This limit applies to a query when it is represented in
!     `normalized form': that is, the constraints have been
!     expressed as a disjunction of conjunctions of relational
!     expressions. The number of relational expressions in a query
!     that has been expanded in this fashion may be greater than
!     the number of relations in the query as orginally written.
!     For example, the expression
!
!             ( ( A LT 1 ) OR ( B GT 2 ) )
!        AND
!             ( ( C NE 3 ) OR ( D EQ 4 ) )
!
!     which contains 4 relational expressions, expands to the
!     equivalent normalized constraint
!
!             (  ( A LT 1 ) AND ( C NE 3 )  )
!        OR
!             (  ( A LT 1 ) AND ( D EQ 4 )  )
!        OR
!             (  ( B GT 2 ) AND ( C NE 3 )  )
!        OR
!             (  ( B GT 2 ) AND ( D EQ 4 )  )
!
!     which contains eight relational expressions.
!
!
   integer               maxcon
   parameter           ( maxcon = 1000 )

!
!     MXJOIN is the maximum number of tables that can be joined.
!
   integer               mxjoin
   parameter           ( mxjoin = 10 )

!
!     MXJCON is the maximum number of join constraints allowed.
!
   integer               mxjcon
   parameter           ( mxjcon = 100 )

!
!     Maximum number of order-by columns that may be used in the
!     `order-by clause' of a query: MAXORD. MAXORD = 10.
!
   integer               maxord
   parameter           ( maxord = 10 )

!
!     Maximum number of tokens in a query: 500. Tokens are reserved
!     words, column names, parentheses, and values. Literal strings
!     and time values count as single tokens.
!
   integer               maxtok
   parameter           ( maxtok = 500 )

!
!     Maximum number of numeric tokens in a query:
!
   integer               maxqnm
   parameter           ( maxqnm = 100 )

!
!     Maximum total length of character tokens in a query:
!
   integer               maxcln
   parameter           ( maxcln = maxqry )

!
!     Maximum length of literal string values allowed in queries:
!     MAXSTR.
!
   integer               maxstr
   parameter           ( maxstr = 1024 )

!
!     End Include Section:  EK Query Limit Parameters
!

end module pepper_inc_ekqlimit
