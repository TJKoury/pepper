C$Procedure F_ST00 ( Tests for string routines, subset 00 )

      SUBROUTINE F_ST00 ( OK )

C$ Abstract
C
C     Perform tests for a subset of the string routines.
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests a subset of the SPICELIB string routines.
C     The subset is:
C
C        CLEARC
C        INSSUB
C        NEXTWD
C        NTHWD
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 17-OCT-2021 (JDR)
C
C-&


C$ Index_Entries
C
C     test string routines, subset 00
C
C-&


C
C     Test Utility Functions
C

C
C     SPICELIB Functions
C

C
C     Local Parameters
C
      CHARACTER*(*)         TSTSTR
      PARAMETER           ( TSTSTR = ' Now is the time,   '
     .                  //  'for all good men     to come.' )

      INTEGER               ARRZS
      PARAMETER           ( ARRZS = 30  )

      INTEGER               LINSZ
      PARAMETER           ( LINSZ = 49  )

      INTEGER               NDIM
      PARAMETER           ( NDIM  = 2   )

      INTEGER               NWORDS
      PARAMETER           ( NWORDS = 10 )

      INTEGER               OUTSZ
      PARAMETER           ( OUTSZ  = 20 )

      INTEGER               WRDSZ
      PARAMETER           ( WRDSZ = 5   )

C
C     Local variables.
C
      CHARACTER*(ARRZS)     ARRAY  ( NDIM )
      CHARACTER*(ARRZS)     DATARR ( NDIM )
      CHARACTER*(WRDSZ)     NEXT
      CHARACTER*(LINSZ)     REST
      CHARACTER*(LINSZ)     STRING
      CHARACTER*(OUTSZ)     OUT
      CHARACTER*(WRDSZ)     WORD
      CHARACTER*(ARRZS)     XARRAY ( NDIM )
      CHARACTER*(WRDSZ)     XPWORD ( NWORDS   )
      CHARACTER*(LINSZ)     XPREST ( NWORDS-1 )

      INTEGER               I
      INTEGER               LOC
      INTEGER               XPLOC  ( NWORDS )

C
C     Test data.
C
      DATA                  DATARR / '0123456789', '0123456789' /

      DATA                  XARRAY / '                              ',
     .                               '                              ' /

      DATA                  XPLOC  /  2,  6,  9, 13, 21,
     .                               25, 29, 34, 42, 45 /

      DATA                  XPWORD / 'Now', 'is',  'the',  'time,',
     .                               'for', 'all', 'good', 'men',
     .                               'to',  'come.'               /

      DATA                  XPREST /
     .                'is the time,   for all good men     to come.',
     .                   'the time,   for all good men     to come.',
     .                       'time,   for all good men     to come.',
     .                               'for all good men     to come.',
     .                                   'all good men     to come.',
     .                                       'good men     to come.',
     .                                            'men     to come.',
     .                                                    'to come.',
     .                                                       'come.' /

C
C     Save all variables.
C
      SAVE


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ST00' )

C
C     *****************************************************************
C
C        Normal cases: CLEARC
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CLEARC: NDIM < 1' )

      ARRAY(1) = DATARR(1)
      ARRAY(2) = DATARR(2)

      CALL CLEARC ( 0, ARRAY )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKAC ( 'ARRAY', ARRAY, '=', DATARR, 2, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CLEARC: Clear an array of 2 dimensions' )

      CALL CLEARC ( NDIM, ARRAY )
      CALL CHCKXC ( .FALSE., ' ',  OK )


      CALL CHCKAC ( 'ARRAY', ARRAY, '=', XARRAY, 2, OK )


C
C     *****************************************************************
C
C        Error cases: INSSUB
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'INSSUB: Insertion before the beginning of the '
     .        //   'string.' )

      CALL INSSUB ( 'Apple a day', 'An ', 0, OUT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'INSSUB: Insertion after the end of the string.' )

      CALL INSSUB ( 'An Apple a day', ' keeps', 16, OUT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)',  OK )

C
C     *****************************************************************
C
C        Normal cases: INSSUB
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C        Lets use WORD, which has a declared size of 5 characters.
C
      CALL TCASE ( 'INSSUB: Insertion happens after the end of output '
     .        //   'string, with different input and output strings.' )

      CALL INSSUB ( '012345', '6789', 6, WORD )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'WORD', WORD, '=', '01234', OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'INSSUB: Insertion happens after the end of output '
     .        //   'string, with equal input and output strings.' )

      WORD = '01234'

      CALL INSSUB ( '012345', '6789', 6, WORD )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'WORD', WORD, '=', '01234', OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'INSSUB: Insertion happens at the beginning of the '
     .        //   'input string.' )

      CALL INSSUB ( ' other woman', 'The', 1, OUT )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'OUT', OUT, '=', 'The other woman     ', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'INSSUB: Insertion happens in the middle of the '
     .        //   'input string.' )

      CALL INSSUB ( 'The rabbit', 'best ', 5, OUT )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'OUT', OUT, '=', 'The best rabbit     ', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'INSSUB: Insertion happens at the end of the input '
     .        //   'string.' )

      CALL INSSUB ( 'An Apple a day', ' keeps', 15, OUT )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'OUT', OUT, '=', 'An Apple a day keeps', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'INSSUB: Insertion causes truncation of original '
     .        //   'substring.' )

      CALL INSSUB ( 'An Apple a day', ' keeps the doctor', 15, OUT )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'OUT', OUT, '=', 'An Apple a day keeps', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'INSSUB: Insertion causes truncation of final '
     .        //   'part of input string.' )

      CALL INSSUB ( 'An a day keeps the', ' Apple', 3, OUT )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'OUT', OUT, '=', 'An Apple a day keeps', OK )


C
C     *****************************************************************
C
C        Normal cases: NEXTWD
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'NEXTWD: Trivial case.' )

      CALL NEXTWD ( ' ', NEXT, REST )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'NEXT', NEXT, '=', ' ', OK )
      CALL CHCKSC ( 'REST', REST, '=', ' ', OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'NEXTWD: Non trivial case.' )

      STRING = TSTSTR

      DO I = 1, NWORDS-1

         CALL NEXTWD ( STRING, NEXT, REST )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSC ( 'NEXT', NEXT, '=', XPWORD(I), OK )
         CALL CHCKSC ( 'REST', REST, '=', XPREST(I), OK )

         STRING = REST

      END DO

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'NEXTWD: Non trivial case. Empty rest string.' )

      CALL NEXTWD ( XPWORD(NWORDS), NEXT, REST )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'NEXT', NEXT, '=', XPWORD(NWORDS), OK )
      CALL CHCKSC ( 'REST', REST, '=', ' ', OK )

C
C     *****************************************************************
C
C        Normal cases: NTHWD
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'NTHWD: Trivial case (blank string)' )

      CALL NTHWD  ( ' ', 10, WORD, LOC )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'WORD', WORD, '=', ' ',   OK )
      CALL CHCKSI ( 'LOC',  LOC,  '=',  0, 0, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'NTHWD: NTH nonpositive' )

      CALL NTHWD  ( TSTSTR, 0, WORD, LOC )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'WORD', WORD, '=', ' ',   OK )
      CALL CHCKSI ( 'LOC',  LOC,  '=',  0, 0, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'NTHWD: Non trivial case.' )

      DO I = 1, NWORDS

         CALL NTHWD  ( TSTSTR, I, WORD, LOC )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSC ( 'WORD', WORD, '=', XPWORD(I),   OK )
         CALL CHCKSI ( 'LOC',  LOC,  '=', XPLOC(I), 0, OK )

      END DO

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'NTHWD: NTH too large -- greater than number of '
     .          // 'words on input string.' )

      CALL NTHWD  ( TSTSTR, 30, WORD, LOC )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'WORD', WORD, '=', ' ',   OK )
      CALL CHCKSI ( 'LOC',  LOC,  '=',  0, 0, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
