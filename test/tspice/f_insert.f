C$Procedure F_INSERT ( SPICE set insertion tests )
 
      SUBROUTINE F_INSERT ( OK )
 
C$ Abstract
C
C     Exercise SPICELIB set insertion routines.
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
C     This routine tests the SPICELIB set insertion routines:
C
C        INSRTC
C        INSRTD
C        INSRTI
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
C-    TSPICE Version 1.0.0, 09-NOV-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CARDC
      INTEGER               CARDD
      INTEGER               CARDI

C
C     Local Parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               ITMWID
      PARAMETER           ( ITMWID = 30 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               CSIZE
      PARAMETER           ( CSIZE  = 300 )

      INTEGER               DSIZE
      PARAMETER           ( DSIZE  = 200 )

      INTEGER               ISIZE
      PARAMETER           ( ISIZE  = 100 )


C
C     Local Variables
C
      CHARACTER*(ITMWID)    CSET   ( LBCELL : CSIZE )
      CHARACTER*(ITMWID)    CVALS  ( CSIZE+1 )
      CHARACTER*(LNSIZE)    LNVALS ( CSIZE )
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(ITMWID)    XITEMC ( CSIZE )

      DOUBLE PRECISION      DSET   ( LBCELL : DSIZE )
      DOUBLE PRECISION      XITEMD ( DSIZE )

      INTEGER               CARD
      INTEGER               I
      INTEGER               ISET   ( LBCELL : ISIZE )
      INTEGER               N
      INTEGER               XITEMI ( ISIZE )

C
C     Saved values
C

C
C     Initial values
C



C
C     Open the test family.
C
      CALL TOPEN ( 'F_INSERT' )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTI test:  insert distinct elements in order.'

      CALL TCASE ( TITLE )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = 1, CARD

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMI( I ) = I

      END DO

      N = CARDI ( ISET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAI ( 'ITEMS', ISET(1), '=', XITEMI, N, OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTI test:  insert non-distinct elements in order.'

      CALL TCASE ( TITLE )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = 1, CARD

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMI( I ) = I

      END DO

      N = CARDI ( ISET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAI ( 'ITEMS', ISET(1), '=', XITEMI, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTI test:  insert distinct elements in reverse ' //
     .        'order.'

      CALL TCASE ( TITLE )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = CARD, 1, -1

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMI( I ) = I

      END DO

      N = CARDI ( ISET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAI ( 'ITEMS', ISET(1), '=', XITEMI, N, OK )



C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTI test:  insert non-distinct elements in reverse '//
     .        'order.'

      CALL TCASE ( TITLE )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = CARD, 1, -1

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMI( I ) = I

      END DO

      N = CARDI ( ISET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAI ( 'ITEMS', ISET(1), '=', XITEMI, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTI test:  insert non-distinct elements in non-'//
     .        'sequential order.'

      CALL TCASE ( TITLE )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = 1, CARD

         CALL INSRTI ( CARD+1-I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMI( I ) = I

      END DO

      N = CARDI ( ISET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAI ( 'ITEMS', ISET(1), '=', XITEMI, N, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INSRTI error case:  set overflow.' )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = 1, CARD

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL INSRTI ( CARD+1, ISET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETEXCESS)', OK )



C*************************************************************
C
C
C     INSRTD tests follow.
C
C
C*************************************************************


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTD test:  insert distinct elements in order.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = 1, CARD

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMD( I ) = I

      END DO

      N = CARDD ( DSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAD ( 'ITEMS', DSET(1), '=', XITEMD, N, 0.D0, OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTD test:  insert non-distinct elements in order.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = 1, CARD

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMD( I ) = I

      END DO

      N = CARDD ( DSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAD ( 'ITEMS', DSET(1), '=', XITEMD, N, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTD test:  insert distinct elements in reverse ' //
     .        'order.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = CARD, 1, -1

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMD( I ) = I

      END DO

      N = CARDD ( DSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAD ( 'ITEMS', DSET(1), '=', XITEMD, N, 0.D0, OK )



C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTD test:  insert non-distinct elements in reverse '//
     .        'order.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = CARD, 1, -1

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMD( I ) = I

      END DO

      N = CARDD ( DSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAD ( 'ITEMS', DSET(1), '=', XITEMD, N, 0.D0, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTD test:  insert non-distinct elements in non-'//
     .        'sequential order.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = 1, CARD

         CALL INSRTD ( DBLE(CARD+1-I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMD( I ) = I

      END DO

      N = CARDD ( DSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAD ( 'ITEMS', DSET(1), '=', XITEMD, N, 0.D0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INSRTD error case:  set overflow.' )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = 1, CARD

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL INSRTD ( DBLE(CARD+1), DSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETEXCESS)', OK )



C*************************************************************
C
C
C     INSRTC tests follow.
C
C
C*************************************************************



C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTC test:  insert distinct elements in order.'

      CALL TCASE ( TITLE )

C
C     Create an array of character set values to be used in place
C     of the numeric values used in the preceding tests.
C
      DO I = 1, CSIZE
C
C        See the description of T_ITHSYM at the bottom of this file.
C
         CALL T_ITHSYM ( I, CSIZE+1, CVALS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        For each element of CVALS, create a longer string that agrees
C        with the first ITMWID characters of the element.
C
         LNVALS(I) = CVALS(I) 

         CALL SUFFIX ( 'YYY', 0, LNVALS(I) )

      END DO

C
C     Create one extra element of CVALS, which will be used later.
C     
      CALL T_ITHSYM ( CSIZE+1, CSIZE+1, CVALS(CSIZE+1) )
      

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTC test:  insert non-distinct elements in order.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTC test:  insert non-distinct elements in order. '//
     .        'This time use duplicate elements that disagree with ' //
     .        'the set elements only at positions past the string '  //
     .        'length of the set.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTC ( LNVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTC test:  insert distinct elements in reverse ' //
     .        'order.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = CARD, 1, -1

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )



C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTC test:  insert non-distinct elements in reverse '//
     .        'order. '                                               //
     .        'This time use duplicate elements that disagree with '  //
     .        'the set elements only at positions past the string '   //
     .        'length of the set.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = CARD, 1, -1

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTC ( LNVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTC test:  insert non-distinct elements in non-'//
     .        'sequential order.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(CARD+1-I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'INSRTC test:  insert non-distinct elements in non-'    //
     .        'sequential order. '                                    //
     .        'This time use duplicate elements that disagree with '  //
     .        'the set elements only at positions past the string '   //
     .        'length of the set.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(CARD+1-I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTC ( LNVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'INSRTC error case:  set overflow.' )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL INSRTC ( CVALS(CARD+1), CSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETEXCESS)', OK )



C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END



C
C     The utility routine T_ITHSYM creates the name of the ith
C     symbol.  The names created by this routine have the form
C  
C        XXX...XXXnnn
C
C     where nnn is the input number N, printed with a sufficient
C     number of leading zeros so that all numbers in the range
C     1:NMAX have the same width.  The number is padded with
C     leading 'X' characters up to the length of the string NAME.
C
C     The calling sequence of the utility is:
C
C        SUBROUTINE T_ITHSYM ( N, NMAX, NAME )
C
C     This utility is defined in the file
C
C        f_symtbi.for
C
