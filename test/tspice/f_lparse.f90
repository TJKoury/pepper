!$Procedure F_LPARSE ( SPICE list parser tests )

      SUBROUTINE F_LPARSE ( OK )

! $ Abstract
!
!     Exercise SPICELIB list parsing routines.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Particulars
!
!     This routine tests the SPICELIB list parsing
!     routines:
!
!        LPARSE
!        LPARSM
!        LPARSS
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 26-OCT-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      INTEGER               CARDC
      INTEGER               RTRIM

!
!     Local Parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               NUMCAS
      PARAMETER           ( NUMCAS = 10 )

      INTEGER               ITMWID
      PARAMETER           ( ITMWID = 30 )

      INTEGER               NARWID
      PARAMETER           ( NARWID = 2 )

      INTEGER               MAXN
      PARAMETER           ( MAXN = 10 )

      INTEGER               MAXSET
      PARAMETER           ( MAXSET = 100 )

      INTEGER               SMALLN
      PARAMETER           ( SMALLN = 2 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Local Variables
!
      CHARACTER*(1)         CHRITM ( MAXN )
      CHARACTER*(1)         DELIM
      CHARACTER*(LNSIZE)    DELIMS
      CHARACTER*(LNSIZE)    LIST
      CHARACTER*(ITMWID)    ITEMS  ( MAXN )
      CHARACTER*(ITMWID)    ITMSET ( LBCELL : MAXSET )
      CHARACTER*(NARWID)    NARITM ( MAXN )
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(ITMWID)    XITEMS ( MAXN )


      INTEGER               N
      INTEGER               NMAX

!
!     Saved values
!

!
!     Initial values
!



!
!     Open the test family.
!
      CALL TOPEN ( 'F_LPARSE' )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  header example 1'

      CALL TCASE ( TITLE )

      LIST      = '  A number of words   separated   by spaces   '

      XITEMS(1) = 'A'
      XITEMS(2) = 'number'
      XITEMS(3) = 'of'
      XITEMS(4) = 'words'
      XITEMS(5) = 'separated'
      XITEMS(6) = 'by'
      XITEMS(7) = 'spaces'

      CALL LPARSE ( LIST, ' ', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 7, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  header example 1, with comma as ' // &
              'delimiter.'

      CALL TCASE ( TITLE )

      XITEMS(1) = 'A number of words   separated   by spaces'

      DELIM = ','
      CALL LPARSE ( LIST, DELIM, MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  header example 2'

      CALL TCASE ( TITLE )

      LIST      = '//option1//option2/ //'

      XITEMS(1) = ' '
      XITEMS(2) = ' '
      XITEMS(3) = 'option1'
      XITEMS(4) = ' '
      XITEMS(5) = 'option2'
      XITEMS(6) = ' '
      XITEMS(7) = ' '
      XITEMS(8) = ' '

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSE ( LIST(:RTRIM(LIST)), '/', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 8, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  header example 3'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'bob'
      XITEMS(3) = 'carol'
      XITEMS(4) = ' '
      XITEMS(5) = 'ted'
      XITEMS(6) = 'alice'

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSE ( LIST(:RTRIM(LIST)), ',', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  header example 3, narrow item array'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'bo'
      XITEMS(3) = 'ca'
      XITEMS(4) = ' '
      XITEMS(5) = 'te'
      XITEMS(6) = 'al'

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSE ( LIST(:RTRIM(LIST)), ',', MAXN, N, NARITM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', NARITM, '=', XITEMS, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  header example 3, 1-char item array'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'b'
      XITEMS(3) = 'c'
      XITEMS(4) = ' '
      XITEMS(5) = 't'
      XITEMS(6) = 'a'

!
!     Use RTRIM to remove trailing blanks from the list.
!
      DELIM = ','

      CALL LPARSE ( LIST(:RTRIM(LIST)), DELIM, MAXN, N, CHRITM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', CHRITM, '=', XITEMS, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  header example 3, ITEMS dim = 2'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'bob'

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSE ( LIST(:RTRIM(LIST)), ',', 2, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  list with single non-blank delimiter'

      CALL TCASE ( TITLE )

      LIST      = ','

      XITEMS(1) = ' '
      XITEMS(2) = ' '

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSE ( LIST(:RTRIM(LIST)), ',', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  blank list, non-blank delimiter'

      CALL TCASE ( TITLE )

      LIST      = ' '

      XITEMS(1) = ' '

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSE ( LIST, ',', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  blank list, blank delimiter'

      CALL TCASE ( TITLE )

      LIST      = ' '

      XITEMS(1) = ' '

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSE ( LIST, ' ', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  consecutive non-blank delimiters'

      CALL TCASE ( TITLE )

      LIST      = ',,,,,,,'

      XITEMS(1) = ' '
      XITEMS(2) = ' '
      XITEMS(3) = ' '
      XITEMS(4) = ' '
      XITEMS(5) = ' '
      XITEMS(6) = ' '
      XITEMS(7) = ' '
      XITEMS(8) = ' '

!
!     Use RTRIM to remove trailing blanks from the list.
!
      DELIM = ','

      CALL LPARSE ( LIST, DELIM, MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 8, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )




!*************************************************************
!
!
!     LPARSM tests follow.
!
!
!*************************************************************

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSM test:  header example 1'

      CALL TCASE ( TITLE )

      LIST      = '  A number of words   separated   by spaces   '

      XITEMS(1) = 'A'
      XITEMS(2) = 'number'
      XITEMS(3) = 'of'
      XITEMS(4) = 'words'
      XITEMS(5) = 'separated'
      XITEMS(6) = 'by'
      XITEMS(7) = 'spaces'

      CALL LPARSM ( LIST, ' ', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 7, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSM test:  header example 2'

      CALL TCASE ( TITLE )

      LIST      = '  1986-187// 13:15:12.184 '

      DELIMS    = ' ,/-:'

!
!     Use NMAX rather than the "standard" MAXN.
!
      NMAX      = 20

      XITEMS(1) = '1986'
      XITEMS(2) = '187'
      XITEMS(3) = ' '
      XITEMS(4) = '13'
      XITEMS(5) = '15'
      XITEMS(6) = '12.184'

      CALL LPARSM ( LIST, DELIMS, NMAX, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSM test:  LPARSE header example 3'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'bob'
      XITEMS(3) = 'carol'
      XITEMS(4) = ' '
      XITEMS(5) = 'ted'
      XITEMS(6) = 'alice'

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSM ( LIST(:RTRIM(LIST)), ',', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSM test:  LPARSE header example 3 with narrow ' // &
              'item array'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'bo'
      XITEMS(3) = 'ca'
      XITEMS(4) = ' '
      XITEMS(5) = 'te'
      XITEMS(6) = 'al'

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSM ( LIST(:RTRIM(LIST)), ',', MAXN, N, NARITM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', NARITM, '=', XITEMS, N, OK )

!

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSM test:  LPARSE header example 3, with blank '// &
              'and comma both considered delimiters'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      DELIMS    = ' ,'

      XITEMS(1) = ' '
      XITEMS(2) = 'bob'
      XITEMS(3) = 'carol'
      XITEMS(4) = ' '
      XITEMS(5) = 'ted'
      XITEMS(6) = 'alice'

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSM ( LIST(:RTRIM(LIST)), DELIMS, MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSE test:  list with single non-blank delimiter'

      CALL TCASE ( TITLE )

      LIST      = ','

      XITEMS(1) = ' '
      XITEMS(2) = ' '

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSM ( LIST(:RTRIM(LIST)), ',', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSM test:  list starting and ending with ' // &
              'single non-blank delimiter'

      CALL TCASE ( TITLE )

      DELIMS    = ',:'
      LIST      = ', :'

      XITEMS(1) = ' '
      XITEMS(2) = ' '
      XITEMS(3) = ' '

!
!     Use RTRIM to remove trailing blanks from the list.
!
      CALL LPARSM ( LIST(:RTRIM(LIST)), DELIMS, MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSM test:  consecutive non-blank delimiters'

      CALL TCASE ( TITLE )

      LIST      = ',,,,,,,'

      XITEMS(1) = ' '
      XITEMS(2) = ' '
      XITEMS(3) = ' '
      XITEMS(4) = ' '
      XITEMS(5) = ' '
      XITEMS(6) = ' '
      XITEMS(7) = ' '
      XITEMS(8) = ' '

!
!     Use RTRIM to remove trailing blanks from the list.
!
      DELIMS = ','

      CALL LPARSM ( LIST, DELIMS, MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 8, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )





!*************************************************************
!
!
!     LPARSS tests follow.
!
!
!*************************************************************


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSS test:  header example 1'

      CALL TCASE ( TITLE )

!
!     Initialize item set.
!
      CALL SSIZEC ( MAXN, ITMSET )



      LIST      = '  A number of words   separated   by spaces.'

      XITEMS(1) = ' '
      XITEMS(2) = 'A'
      XITEMS(3) = 'by'
      XITEMS(4) = 'number'
      XITEMS(5) = 'of'
      XITEMS(6) = 'separated'
      XITEMS(7) = 'spaces'
      XITEMS(8) = 'words'

      DELIMS      = ' ,.'

      CALL LPARSS ( LIST, DELIMS, ITMSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N = CARDC(ITMSET)

      CALL CHCKSI ( 'CARDC(ITMSET)', N, '=', 8, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITMSET(1), '=', XITEMS, N, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSS test:  header example 2'

      CALL TCASE ( TITLE )

      LIST      = '  1986-187// 13:15:12.184 '
      DELIMS    = ' ,/-:'


      XITEMS(1) = ' '
      XITEMS(2) = '12.184'
      XITEMS(3) = '13'
      XITEMS(4) = '15'
      XITEMS(5) = '187'
      XITEMS(6) = '1986'


      CALL LPARSS ( LIST, DELIMS, ITMSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N = CARDC(ITMSET)

      CALL CHCKSI ( 'CARDC(ITMSET)', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITMSET(1), '=', XITEMS, N, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSS test:  header example 3'

      CALL TCASE ( TITLE )

      LIST        = '  ,This,  is, ,an,, example, '
      DELIMS      = ' ,'

      XITEMS(1) = ' '
      XITEMS(2) = 'This'
      XITEMS(3) = 'an'
      XITEMS(4) = 'example'
      XITEMS(5) = 'is'


      CALL LPARSS ( LIST, DELIMS, ITMSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N = CARDC(ITMSET)

      CALL CHCKSI ( 'CARDC(ITMSET)', N, '=', 5, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITMSET(1), '=', XITEMS, N, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSS test:  header example 4'

      CALL TCASE ( TITLE )

      LIST        = 'Mary had a little lamb, little lamb ' // &
                    'whose fleece was white      as snow.'
      DELIMS      = ' ,.'

      CALL SSIZEC ( 6, ITMSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LPARSS ( LIST, DELIMS, ITMSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETEXCESS)', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSS test:  header example 5'

      CALL TCASE ( TITLE )

      LIST        = '1 2 3 4 5 6 7 8 9 10.'
      DELIMS      = ' .'

      CALL SSIZEC ( 10, ITMSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LPARSS ( LIST, DELIMS, ITMSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETEXCESS)', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'LPARSS test:  header example 6'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( 100, ITMSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      LIST        = '1 2 3 4 5 6 7 8 9 10.'
      DELIMS      = '.'

      XITEMS(1) = ' '
      XITEMS(2) = '1 2 3 4 5 6 7 8 9 10'


      CALL LPARSS ( LIST, DELIMS(:RTRIM(DELIMS)), ITMSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      N = CARDC(ITMSET)

      CALL CHCKSI ( 'CARDC(ITMSET)', N, '=', 2, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITMSET(1), '=', XITEMS, N, OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


