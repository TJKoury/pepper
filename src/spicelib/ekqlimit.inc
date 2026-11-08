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
!
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
      INTEGER               MAXQRY
      PARAMETER           ( MAXQRY = 2000 )

!
!     Maximum number of columns that may be listed in the
!     `order-by clause' of a query:  MAXSEL.  MAXSEL = 50.
!
      INTEGER               MAXSEL
      PARAMETER           ( MAXSEL = 50 )

!
!     Maximum number of tables that may be listed in the `FROM
!     clause' of a query: MAXTAB.
!
      INTEGER               MAXTAB
      PARAMETER           ( MAXTAB = 10 )

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
      INTEGER               MAXCON
      PARAMETER           ( MAXCON = 1000 )

!
!     MXJOIN is the maximum number of tables that can be joined.
!
      INTEGER               MXJOIN
      PARAMETER           ( MXJOIN = 10 )

!
!     MXJCON is the maximum number of join constraints allowed.
!
      INTEGER               MXJCON
      PARAMETER           ( MXJCON = 100 )

!
!     Maximum number of order-by columns that may be used in the
!     `order-by clause' of a query: MAXORD. MAXORD = 10.
!
      INTEGER               MAXORD
      PARAMETER           ( MAXORD = 10 )

!
!     Maximum number of tokens in a query: 500. Tokens are reserved
!     words, column names, parentheses, and values. Literal strings
!     and time values count as single tokens.
!
      INTEGER               MAXTOK
      PARAMETER           ( MAXTOK = 500 )

!
!     Maximum number of numeric tokens in a query:
!
      INTEGER               MAXQNM
      PARAMETER           ( MAXQNM = 100 )

!
!     Maximum total length of character tokens in a query:
!
      INTEGER               MAXCLN
      PARAMETER           ( MAXCLN = MAXQRY )

!
!     Maximum length of literal string values allowed in queries:
!     MAXSTR.
!
      INTEGER               MAXSTR
      PARAMETER           ( MAXSTR = 1024 )

!
!     End Include Section:  EK Query Limit Parameters
!
