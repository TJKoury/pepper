C$Procedure F_LPARSE ( SPICE list parser tests )
 
      SUBROUTINE F_LPARSE ( OK )
 
C$ Abstract
C
C     Exercise SPICELIB list parsing routines.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Particulars
C
C     This routine tests the SPICELIB list parsing 
C     routines:
C
C        LPARSE
C        LPARSM
C        LPARSS
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 1.0.0, 26-OCT-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               RTRIM

C
C     Local Parameters
C
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

C
C     Local Variables
C
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

C
C     Saved values
C

C
C     Initial values
C



C
C     Open the test family.
C
      CALL TOPEN ( 'F_LPARSE' )

C
C --- Case: ------------------------------------------------------
C
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

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSE test:  header example 1, with comma as ' //
     .        'delimiter.'

      CALL TCASE ( TITLE )

      XITEMS(1) = 'A number of words   separated   by spaces'

      DELIM = ','
      CALL LPARSE ( LIST, DELIM, MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


C
C --- Case: ------------------------------------------------------
C
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

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSE ( LIST(:RTRIM(LIST)), '/', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 8, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )



C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSE test:  header example 3'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'bob'
      XITEMS(3) = 'carol'
      XITEMS(4) = ' '
      XITEMS(5) = 'ted'
      XITEMS(6) = 'alice'

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSE ( LIST(:RTRIM(LIST)), ',', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSE test:  header example 3, narrow item array'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'bo'
      XITEMS(3) = 'ca'
      XITEMS(4) = ' '
      XITEMS(5) = 'te'
      XITEMS(6) = 'al'

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSE ( LIST(:RTRIM(LIST)), ',', MAXN, N, NARITM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', NARITM, '=', XITEMS, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSE test:  header example 3, 1-char item array'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'b'
      XITEMS(3) = 'c'
      XITEMS(4) = ' '
      XITEMS(5) = 't'
      XITEMS(6) = 'a'

C
C     Use RTRIM to remove trailing blanks from the list.
C
      DELIM = ','

      CALL LPARSE ( LIST(:RTRIM(LIST)), DELIM, MAXN, N, CHRITM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', CHRITM, '=', XITEMS, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSE test:  header example 3, ITEMS dim = 2'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'bob'

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSE ( LIST(:RTRIM(LIST)), ',', 2, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSE test:  list with single non-blank delimiter'

      CALL TCASE ( TITLE )

      LIST      = ','

      XITEMS(1) = ' '
      XITEMS(2) = ' '

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSE ( LIST(:RTRIM(LIST)), ',', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSE test:  blank list, non-blank delimiter'

      CALL TCASE ( TITLE )

      LIST      = ' '

      XITEMS(1) = ' '

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSE ( LIST, ',', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSE test:  blank list, blank delimiter'

      CALL TCASE ( TITLE )

      LIST      = ' '

      XITEMS(1) = ' '

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSE ( LIST, ' ', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


C
C --- Case: ------------------------------------------------------
C
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

C
C     Use RTRIM to remove trailing blanks from the list.
C
      DELIM = ','

      CALL LPARSE ( LIST, DELIM, MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 8, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )




C*************************************************************
C
C
C     LPARSM tests follow.
C
C
C*************************************************************

C
C --- Case: ------------------------------------------------------
C
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

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSM test:  header example 2'

      CALL TCASE ( TITLE )

      LIST      = '  1986-187// 13:15:12.184 '

      DELIMS    = ' ,/-:'

C
C     Use NMAX rather than the "standard" MAXN.
C      
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


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSM test:  LPARSE header example 3'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'bob'
      XITEMS(3) = 'carol'
      XITEMS(4) = ' '
      XITEMS(5) = 'ted'
      XITEMS(6) = 'alice'

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSM ( LIST(:RTRIM(LIST)), ',', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSM test:  LPARSE header example 3 with narrow ' //
     .        'item array'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      XITEMS(1) = ' '
      XITEMS(2) = 'bo'
      XITEMS(3) = 'ca'
      XITEMS(4) = ' '
      XITEMS(5) = 'te'
      XITEMS(6) = 'al'

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSM ( LIST(:RTRIM(LIST)), ',', MAXN, N, NARITM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', NARITM, '=', XITEMS, N, OK )

C

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSM test:  LPARSE header example 3, with blank '//
     .        'and comma both considered delimiters'

      CALL TCASE ( TITLE )

      LIST      = ' ,bob,   carol,, ted,  alice'

      DELIMS    = ' ,'

      XITEMS(1) = ' '
      XITEMS(2) = 'bob'
      XITEMS(3) = 'carol'
      XITEMS(4) = ' '
      XITEMS(5) = 'ted'
      XITEMS(6) = 'alice'

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSM ( LIST(:RTRIM(LIST)), DELIMS, MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSE test:  list with single non-blank delimiter'

      CALL TCASE ( TITLE )

      LIST      = ','

      XITEMS(1) = ' '
      XITEMS(2) = ' '

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSM ( LIST(:RTRIM(LIST)), ',', MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSM test:  list starting and ending with ' //
     .        'single non-blank delimiter'

      CALL TCASE ( TITLE )

      DELIMS    = ',:'
      LIST      = ', :'

      XITEMS(1) = ' '
      XITEMS(2) = ' '
      XITEMS(3) = ' '

C
C     Use RTRIM to remove trailing blanks from the list.
C
      CALL LPARSM ( LIST(:RTRIM(LIST)), DELIMS, MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )



C
C --- Case: ------------------------------------------------------
C
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

C
C     Use RTRIM to remove trailing blanks from the list.
C
      DELIMS = ','

      CALL LPARSM ( LIST, DELIMS, MAXN, N, ITEMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 8, 0, OK )

      CALL CHCKAC ( 'ITEMS', ITEMS, '=', XITEMS, N, OK )





C*************************************************************
C
C
C     LPARSS tests follow.
C
C
C*************************************************************


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSS test:  header example 1'

      CALL TCASE ( TITLE )

C
C     Initialize item set.
C
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



C
C --- Case: ------------------------------------------------------
C
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



C
C --- Case: ------------------------------------------------------
C
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



C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSS test:  header example 4'

      CALL TCASE ( TITLE )

      LIST        = 'Mary had a little lamb, little lamb ' //
     .              'whose fleece was white      as snow.'
      DELIMS      = ' ,.'

      CALL SSIZEC ( 6, ITMSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LPARSS ( LIST, DELIMS, ITMSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETEXCESS)', OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'LPARSS test:  header example 5'

      CALL TCASE ( TITLE )

      LIST        = '1 2 3 4 5 6 7 8 9 10.'
      DELIMS      = ' .'

      CALL SSIZEC ( 10, ITMSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LPARSS ( LIST, DELIMS, ITMSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETEXCESS)', OK )


C
C --- Case: ------------------------------------------------------
C
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

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


