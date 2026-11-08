!$Procedure F_ST00 ( Tests for string routines, subset 00 )

      SUBROUTINE F_ST00 ( OK )

! $ Abstract
!
!     Perform tests for a subset of the string routines.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests a subset of the SPICELIB string routines.
!     The subset is:
!
!        CLEARC
!        INSSUB
!        NEXTWD
!        NTHWD
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 17-OCT-2021 (JDR)
!
!-&


! $ Index_Entries
!
!     test string routines, subset 00
!
!-&


!
!     Test Utility Functions
!

!
!     SPICELIB Functions
!

!
!     Local Parameters
!
      CHARACTER*(*)         TSTSTR
      PARAMETER           ( TSTSTR = ' Now is the time,   ' &
                        //  'for all good men     to come.' )

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

!
!     Local variables.
!
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

!
!     Test data.
!
      DATA                  DATARR / '0123456789', '0123456789' /

      DATA                  XARRAY / '                              ', &
                                     '                              ' /

      DATA                  XPLOC  /  2,  6,  9, 13, 21, &
                                     25, 29, 34, 42, 45 /

      DATA                  XPWORD / 'Now', 'is',  'the',  'time,', &
                                     'for', 'all', 'good', 'men', &
                                     'to',  'come.'               /

      DATA                  XPREST / &
                      'is the time,   for all good men     to come.', &
                         'the time,   for all good men     to come.', &
                             'time,   for all good men     to come.', &
                                     'for all good men     to come.', &
                                         'all good men     to come.', &
                                             'good men     to come.', &
                                                  'men     to come.', &
                                                          'to come.', &
                                                             'come.' /

!
!     Save all variables.
!
      SAVE


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ST00' )

!
!     *****************************************************************
!
!        Normal cases: CLEARC
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CLEARC: NDIM < 1' )

      ARRAY(1) = DATARR(1)
      ARRAY(2) = DATARR(2)

      CALL CLEARC ( 0, ARRAY )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKAC ( 'ARRAY', ARRAY, '=', DATARR, 2, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CLEARC: Clear an array of 2 dimensions' )

      CALL CLEARC ( NDIM, ARRAY )
      CALL CHCKXC ( .FALSE., ' ',  OK )


      CALL CHCKAC ( 'ARRAY', ARRAY, '=', XARRAY, 2, OK )


!
!     *****************************************************************
!
!        Error cases: INSSUB
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'INSSUB: Insertion before the beginning of the ' &
              //   'string.' )

      CALL INSSUB ( 'Apple a day', 'An ', 0, OUT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'INSSUB: Insertion after the end of the string.' )

      CALL INSSUB ( 'An Apple a day', ' keeps', 16, OUT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)',  OK )

!
!     *****************************************************************
!
!        Normal cases: INSSUB
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!        Lets use WORD, which has a declared size of 5 characters.
!
      CALL TCASE ( 'INSSUB: Insertion happens after the end of output ' &
              //   'string, with different input and output strings.' )

      CALL INSSUB ( '012345', '6789', 6, WORD )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'WORD', WORD, '=', '01234', OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'INSSUB: Insertion happens after the end of output ' &
              //   'string, with equal input and output strings.' )

      WORD = '01234'

      CALL INSSUB ( '012345', '6789', 6, WORD )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'WORD', WORD, '=', '01234', OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'INSSUB: Insertion happens at the beginning of the ' &
              //   'input string.' )

      CALL INSSUB ( ' other woman', 'The', 1, OUT )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'OUT', OUT, '=', 'The other woman     ', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'INSSUB: Insertion happens in the middle of the ' &
              //   'input string.' )

      CALL INSSUB ( 'The rabbit', 'best ', 5, OUT )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'OUT', OUT, '=', 'The best rabbit     ', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'INSSUB: Insertion happens at the end of the input ' &
              //   'string.' )

      CALL INSSUB ( 'An Apple a day', ' keeps', 15, OUT )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'OUT', OUT, '=', 'An Apple a day keeps', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'INSSUB: Insertion causes truncation of original ' &
              //   'substring.' )

      CALL INSSUB ( 'An Apple a day', ' keeps the doctor', 15, OUT )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'OUT', OUT, '=', 'An Apple a day keeps', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'INSSUB: Insertion causes truncation of final ' &
              //   'part of input string.' )

      CALL INSSUB ( 'An a day keeps the', ' Apple', 3, OUT )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'OUT', OUT, '=', 'An Apple a day keeps', OK )


!
!     *****************************************************************
!
!        Normal cases: NEXTWD
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'NEXTWD: Trivial case.' )

      CALL NEXTWD ( ' ', NEXT, REST )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'NEXT', NEXT, '=', ' ', OK )
      CALL CHCKSC ( 'REST', REST, '=', ' ', OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'NEXTWD: Non trivial case.' )

      STRING = TSTSTR

      DO I = 1, NWORDS-1

         CALL NEXTWD ( STRING, NEXT, REST )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSC ( 'NEXT', NEXT, '=', XPWORD(I), OK )
         CALL CHCKSC ( 'REST', REST, '=', XPREST(I), OK )

         STRING = REST

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'NEXTWD: Non trivial case. Empty rest string.' )

      CALL NEXTWD ( XPWORD(NWORDS), NEXT, REST )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'NEXT', NEXT, '=', XPWORD(NWORDS), OK )
      CALL CHCKSC ( 'REST', REST, '=', ' ', OK )

!
!     *****************************************************************
!
!        Normal cases: NTHWD
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'NTHWD: Trivial case (blank string)' )

      CALL NTHWD  ( ' ', 10, WORD, LOC )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'WORD', WORD, '=', ' ',   OK )
      CALL CHCKSI ( 'LOC',  LOC,  '=',  0, 0, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'NTHWD: NTH nonpositive' )

      CALL NTHWD  ( TSTSTR, 0, WORD, LOC )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'WORD', WORD, '=', ' ',   OK )
      CALL CHCKSI ( 'LOC',  LOC,  '=',  0, 0, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'NTHWD: Non trivial case.' )

      DO I = 1, NWORDS

         CALL NTHWD  ( TSTSTR, I, WORD, LOC )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSC ( 'WORD', WORD, '=', XPWORD(I),   OK )
         CALL CHCKSI ( 'LOC',  LOC,  '=', XPLOC(I), 0, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'NTHWD: NTH too large -- greater than number of ' &
                // 'words on input string.' )

      CALL NTHWD  ( TSTSTR, 30, WORD, LOC )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSC ( 'WORD', WORD, '=', ' ',   OK )
      CALL CHCKSI ( 'LOC',  LOC,  '=',  0, 0, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
