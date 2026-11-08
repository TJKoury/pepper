C$Procedure      F_ZZTIME ( Family of tests for ZZTIME )
 
      SUBROUTINE F_ZZTIME ( OK )
  
C$ Abstract
C
C     This is a family of tests for the private routine ZZTIME.
C
C     Part of the utility of ZZTIME is it's ability to diagnose
C     problems with an input time string.  Although it is probably
C     not desirable to construct expected diagnostic strings, it
C     is useful to review them.  This test family has incorporated
C     test cases designed to produce every type of diagnostic that
C     can be returned from ZZUNPCK.  To see these, you should
C     add the following parameter to the command line when
C     you run the parent test program that calls F_ZZTIME
C
C        T_<program> [verbosity options] -diags
C
C     The diagnostics will then be printed on the screen and in
C     the output log file allowing you to inspect these strings
C     for grammar, spelling, etc.
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
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the low-level SPICELIB time system utility
C     suite ZZTIME and its entry points.
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
C     N.J. Bachman     (JPL)
C     B.V. Semenov     (JPL)
C     W.L. Taber       (JPL)
C
C$ Version
C
C-    TSPICE Version 3.1.0, 05-FEB-2014 (BVS)
C
C        Added tests for ZZTOKNS token buffer and picture overflow
C        exceptions.
C
C-    TSPICE Version 3.0.0, 08-MAR-2009 (NJB)
C
C        Added tests for detection of non-printing characters
C        with ASCII codes 127, 128.
C
C-    TSPICE Version 2.0.0, 27-OCT-2006 (BVS)
C
C        Added ZZTOKNS tests with blank input strings.
C
C-    TSPICE Version 1.0.0, 01-JAN-2000 (WLT)
C
C-&
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
 
      LOGICAL               ZZTOKNS
      LOGICAL               ZZGREP
      LOGICAL               ZZNOTE
      LOGICAL               ZZREMT
      LOGICAL               ZZCMBT
      LOGICAL               ZZSUBT
      LOGICAL               ZZISPT
      LOGICAL               ZZIST
      LOGICAL               ZZVALT
      LOGICAL               ZZUNPCK
      LOGICAL               ZZREPT

C
C     Parameters from ZZTIME: maximum number of tokens that a valid 
C     time string can contain and length of the string buffer 
C     containing the time string picture.
C
      INTEGER               MAXTKN
      PARAMETER           ( MAXTKN = 64 )

      INTEGER               PICLEN
      PARAMETER           ( PICLEN = MAXTKN * 5 )

C
C     Local Variables
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = MAXTKN )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = PICLEN )
 
      INTEGER               ERRSIZ
      PARAMETER           ( ERRSIZ = PICLEN * 2 )
 
 
      CHARACTER*(WDSIZE)    PIECES ( 87 )
      CHARACTER*(WDSIZE)    W1
      CHARACTER*(WDSIZE)    W2
      CHARACTER*(2)         M1
      CHARACTER*(2)         M2
      CHARACTER*(LNSIZE)    CMLINE
      CHARACTER*(ERRSIZ)    STRING
      CHARACTER*(LNSIZE)    MSTYLE
      CHARACTER*(LNSIZE)    GOOD
      CHARACTER*(LNSIZE)    BAD
      CHARACTER*(LNSIZE)    PIC
      CHARACTER*(ERRSIZ)    ERROR
      CHARACTER*(ERRSIZ)    EXPERR
      CHARACTER*(ERRSIZ)    TKNERR
      CHARACTER*(ERRSIZ)    PICERR
      CHARACTER*(WDSIZE)    REP
      CHARACTER*(WDSIZE)    EREP
 
      CHARACTER*(WDSIZE)    TRANSL
      CHARACTER*(WDSIZE)    ETRANS
      CHARACTER*(1)         ONECHR
 
      DOUBLE PRECISION      TVEC ( 10 )
      DOUBLE PRECISION      ETVEC( 10 )
 
      INTEGER               EXPN
      INTEGER               I
      INTEGER               J
      INTEGER               B
      INTEGER               E
      INTEGER               NTVEC
      
      INTEGER               LAST
 
      LOGICAL               SHDIAG
      LOGICAL               DID
      LOGICAL               GOT
      LOGICAL               L2R
      LOGICAL               R2L
      LOGICAL               YABBRV
 
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZTIME' )
 
      MSTYLE = 'LEFT 5 RIGHT 75 FLAG Diagnostic: '
      L2R    = .TRUE.
      R2L    = .NOT. L2R
      YABBRV = .FALSE.
 
      CALL GETCML ( CMLINE )
      CALL UCASE  ( CMLINE, CMLINE )
 
      SHDIAG = INDEX ( CMLINE, '-DIAGS' ) .GT. 0
 
 
      CALL TCASE ( 'Check that that tokenization process behaves '
     .//           'as expected. ' )
 
 
         PIECES( 1) = 'PDT Z'
         PIECES( 2) = 'pst Z'
         PIECES( 3) = 'CDT Z'
         PIECES( 4) = 'CST Z'
         PIECES( 5) = 'MST Z'
         PIECES( 6) = 'MDT Z'
         PIECES( 7) = 'EST Z'
         PIECES( 8) = 'EDT Z'
         PIECES( 9) = 'UTC+ O'
         PIECES(10) = 'UTC- o'
         PIECES(11) = 'TDT s'
         PIECES(12) = 'TDB s'
         PIECES(13) = 'JD j'
         PIECES(14) = 'JD j'
         PIECES(15) = '( ['
         PIECES(16) = ') ]'
         PIECES(17) = '. .'
         PIECES(18) = ': :'
         PIECES(19) = ':: d'
         PIECES(20) = '// d'
         PIECES(21) = '/ /'
         PIECES(22) = 'January m'
         PIECES(23) = 'February m'
         PIECES(24) = 'March m'
         PIECES(25) = 'April m'
         PIECES(26) = 'May m'
         PIECES(27) = 'June m'
         PIECES(28) = 'July m'
         PIECES(29) = 'August m'
         PIECES(30) = 'September m'
         PIECES(31) = 'October m'
         PIECES(32) = 'November m'
         PIECES(33) = 'December m'
         PIECES(34) = 'jan, m,'
         PIECES(35) = 'feb m'
         PIECES(36) = 'mar, m,'
         PIECES(37) = 'apr m'
         PIECES(38) = 'may m'
         PIECES(39) = 'jun, m,'
         PIECES(40) = 'jul m'
         PIECES(41) = 'aug, m,'
         PIECES(42) = 'sept, m,'
         PIECES(43) = 'oct, m,'
         PIECES(44) = 'nov, m,'
         PIECES(45) = 'dec, m,'
         PIECES(46) = 'mon. w.'
         PIECES(47) = 'tues. w.'
         PIECES(48) = 'wed. w.'
         PIECES(49) = 'thu w'
         PIECES(50) = 'fri. w.'
         PIECES(51) = 'sat. w.'
         PIECES(52) = 'sun. w.'
         PIECES(53) = 'a.d. e'
         PIECES(54) = 'ad e'
         PIECES(55) = 'b.c. e'
         PIECES(56) = 'bc e'
         PIECES(57) = '1768 i'
         PIECES(58) = '12 i'
         PIECES(59) = '1 i'
         PIECES(60) = '2 i'
         PIECES(61) = '3 i'
         PIECES(62) = '4 i'
         PIECES(63) = '5 i'
         PIECES(64) = '6 i'
         PIECES(65) = '7 i'
         PIECES(66) = '8 i'
         PIECES(67) = '9 i'
         PIECES(68) = '0 i'
         PIECES(69) = '10 i'
         PIECES(70) = '21 i'
         PIECES(71) = '22 i'
         PIECES(72) = '23 i'
         PIECES(73) = '24 i'
         PIECES(74) = '25 i'
         PIECES(75) = '. .'
         PIECES(76) = ', ,'
         PIECES(77) = ''' '''
         PIECES(78) = '- -'
         PIECES(79) = 'am N'
         PIECES(80) = 'a.m. N'
         PIECES(81) = 'pm N'
         PIECES(82) = 'p.m. N'
         PIECES(83) = 't t'
         PIECES(84) = 'T t'
         PIECES(85) = 'TDB s'
         PIECES(86) = 'TDT s'
         PIECES(87) = 'UTC s'
 
         DO I = 80, 87
 
            CALL NEXTWD ( PIECES(I), W1, M1 )
 
            DO J = 1, 20
               CALL NEXTWD ( PIECES(J), W2, M2 )
 
               EREP   = M1
               CALL SUFFIX ( M2, 0, EREP )
 
               STRING = W1
               CALL SUFFIX ( W2, 0, STRING )
 
               IF ( EREP .EQ. 'ii' ) THEN
                  EREP = 'i'
               ELSE IF ( STRING .EQ. '::' ) THEN
                  EREP = 'd'
               ELSE IF ( STRING .EQ. ':::' ) THEN
                  EREP = 'd:'
               ELSE IF ( STRING .EQ. '//' ) THEN
                  EREP = 'd'
               ELSE IF ( STRING .EQ. '///' ) THEN
                  EREP = 'd/'
               ELSE IF ( STRING .EQ. 'UTC-' ) THEN
                  EREP = 'o'
               END IF
 
               DID = ZZTOKNS( STRING, ERROR )
               GOT = ZZGREP ( REP )
 
               CALL TSTMSG ( '#', 'The value of string was: "#"' )
               CALL TSTMSC ( STRING )
 
               CALL CHCKXC ( .FALSE., ' ',              OK )
               CALL CHCKSL ( 'DID',    DID, .TRUE.,     OK )
               CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
               CALL CHCKSC ( 'REP',    REP,  '=', EREP, OK )
 
               EREP   = M1
               CALL SUFFIX ( 'b', 0, EREP )
               CALL SUFFIX ( M2,  0, EREP )
               STRING = W1
 
               CALL SUFFIX ( W2, 3, STRING )
               DID = ZZTOKNS( STRING, ERROR )
               GOT = ZZGREP ( REP )
 
               CALL TSTMSG ( '#', 'The value of string was: "#"' )
               CALL TSTMSC ( STRING )
               CALL CHCKXC ( .FALSE., ' ',              OK )
               CALL CHCKSL ( 'DID',    DID, .TRUE.,     OK )
               CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
               CALL CHCKSC ( 'REP',    REP, '=',  EREP, OK )
            END DO
 
         END DO
 
         CALL TSTMSG ( ' ', ' ' )
 
      CALL TCASE ( 'Make sure that ZZNOTE can retrieve '
     .//           'components from a date properly and that the '
     .//           'representation is properly reduced. ' )
 
 
         STRING = '12 JAN 1992 A.D., 11:12:18 P.M.'
         EREP   = 'ibmbibe,bi:i:ibN'
 
         ERROR  = 'xxx'
 
         DID    = ZZTOKNS ( STRING, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'DID',    DID, .TRUE.,     OK )
         CALL CHCKSC ( 'ERROR',  ERROR, '=', ' ', OK )
 
         DID    = ZZGREP ( REP )
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'DID',    DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP',    REP, '=', EREP,  OK )
 
         GOT    = ZZNOTE ( 'e', B, E )
         DID    = ZZGREP ( REP )
         EREP   = 'ibmbib,bi:i:ibN'
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID',    DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP',    REP, '=', EREP,  OK )
         CALL CHCKSC ( 'STRING(B:E)', STRING(B:E), '=', 'A.D.', OK )
 
 
      CALL TCASE ( 'Make sure that items are properly removed by '
     .//           'calls to ZZREMT and that ZZNOTE can still '
     .//           'locate tokens after such removals. ' )
 
 
         STRING = ' 12 JAN 1992 A.D., 11:12:18 P.M.'
         EREP   = 'bibmbibe,bi:i:ibN'
 
         ERROR  = 'xxx'
 
         DID    = ZZTOKNS ( STRING, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'DID1',    DID, .TRUE.,     OK )
         CALL CHCKSC ( 'ERROR',  ERROR, '=', ' ', OK )
 
         DID    = ZZGREP ( REP )
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'DID2',    DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP',    REP, '=', EREP,  OK )
 
         GOT    = ZZREMT ( 'b' )
         DID    = ZZGREP ( REP )
         EREP   = 'imie,i:i:iN'
 
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID3',    DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP',    REP, '=', EREP,  OK )
 
         GOT    = ZZREMT ( 'b' )
         DID    = ZZGREP ( REP )
         EREP   = 'imie,i:i:iN'
 
         CALL CHCKSL ( 'GOT',    GOT, .FALSE.,    OK )
         CALL CHCKSL ( 'DID4',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP',    REP, '=', EREP,  OK )
 
         GOT    = ZZNOTE ( 'e', B, E )
         DID    = ZZGREP ( REP )
         EREP   = 'imi,i:i:iN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID5',    DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP',    REP, '=', EREP,  OK )
         CALL CHCKSC ( 'STRING(B:E)', STRING(B:E), '=', 'A.D.', OK )
 
      CALL TCASE ( 'Make sure that tokens can be combined '
     .//           'successfully. ' )
 
 
         STRING = ' 12 JAN 1992 A.D., 11:12:18 P.M.'
         EREP   = 'bibmbibe,bi:i:ibN'
 
         ERROR  = 'xxx'
C
C        First tokenize the string and make sure it has the
C        expected tokenization.
C
         DID    = ZZTOKNS ( STRING, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'DID1',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'ERROR',  ERROR, '=', ' ', OK )
 
         DID    = ZZGREP ( REP )
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'DID2',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP1',   REP, '=', EREP,  OK )
C
C        Now remove 'b' from the tokenization and check the
C        representation.
C
         GOT    = ZZREMT ( 'b' )
         DID    = ZZGREP ( REP )
         EREP   = 'imie,i:i:iN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID3',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP2',   REP, '=', EREP,  OK )
C
C        Combine a few tokens and see if we have the expected
C        representation.
C
         GOT    = ZZCMBT ( 'i:i', 'K', R2L )
         DID    = ZZGREP ( REP )
         EREP   = 'imie,i:KN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID4',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP3',   REP, '=', EREP,  OK )
C
C        See if the 'K' in the current tokenization maps to the
C        expected substring and check the NOTEd representation.
C
         GOT    = ZZNOTE ( 'K', B, E )
         DID    = ZZGREP ( REP )
         EREP   = 'imie,i:N'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID5',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP4',   REP, '=', EREP,  OK )
         CALL CHCKSC ( 'STRING(B:E)', STRING(B:E), '=',
     .                 '12:18', OK )
 
C
C        Combine tokens again and go through the same steps as
C        the last block of code.
C
         GOT    = ZZCMBT ( 'imie', 'K', L2R )
         DID    = ZZGREP ( REP )
         EREP   = 'K,i:N'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID6',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP5',   REP, '=', EREP,  OK )
 
         GOT    = ZZNOTE ( 'K', B, E )
         DID    = ZZGREP ( REP )
         EREP   = ',i:N'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID7',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP6',   REP, '=', EREP,  OK )
         CALL CHCKSC ( 'STRING(B:E)', STRING(B:E), '=',
     .                 '12 JAN 1992 A.D.', OK )
C
C        Now try combining something that's not present.
C
         GOT    = ZZCMBT ( 'i:i', 'K', L2R )
         DID    = ZZGREP ( REP )
         EREP   = ',i:N'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .FALSE.,     OK )
         CALL CHCKSL ( 'DID6',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP5',   REP, '=', EREP,  OK )
 
         GOT    = ZZNOTE ( 'K', B, E )
         DID    = ZZGREP ( REP )
         EREP   = ',i:N'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .FALSE.,    OK )
         CALL CHCKSL ( 'DID7',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP6',   REP, '=', EREP,  OK )
         CALL CHCKSI ( 'B',      B,   '=', 0, 0,  OK )
         CALL CHCKSI ( 'E',      E,   '=', 0, 0,  OK )
 
      CALL TCASE ( 'Make sure that the entry point ZZSUBT works '
     .//           'as expected. ' )
 
 
         STRING = ' 12 JAN 1992 A.D., 11:12:18 P.M.'
C
C        First tokenize the string and remove blanks.
C
         DID    = ZZTOKNS ( STRING, ERROR )
         GOT    = ZZREMT ( 'b' )
         DID    = ZZGREP ( REP )
         EREP   = 'imie,i:i:iN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID1',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP1',   REP, '=', EREP,  OK )
C
C        Now perform a left to right substitution.
C
         GOT    = ZZSUBT ( 'i:i', 'H:M', L2R )
         DID    = ZZGREP ( REP )
         EREP   = 'imie,H:M:iN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID2',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP2',   REP, '=', EREP,  OK )
C
C        Try another left to right substitution, this should
C        turn up with no substitution.
C
         GOT    = ZZSUBT ( 'i:i', 'M:S', L2R )
         DID    = ZZGREP ( REP )
         EREP   = 'imie,H:M:iN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .FALSE.,    OK )
         CALL CHCKSL ( 'DID3',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP3',   REP, '=', EREP,  OK )
C
C        Get the value associated with H and make sure
C        it's the correct value.
C
         GOT    = ZZNOTE ( 'H', B, E )
         DID    = ZZGREP ( REP )
         EREP   = 'imie,:M:iN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID4',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP4',   REP, '=', EREP,  OK )
         CALL CHCKSC ( 'STRING(B:E)', STRING(B:E), '=', '11', OK )
C
C        Perform a right to left substitution and make sure the
C        representation changes as expected.
C
         GOT    = ZZSUBT ( 'imi', 'YmD', R2L )
         DID    = ZZGREP ( REP )
         EREP   = 'YmDe,:M:iN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',    GOT, .TRUE.,     OK )
         CALL CHCKSL ( 'DID5',   DID, .TRUE.,     OK )
         CALL CHCKSC ( 'REP5',   REP, '=', EREP,  OK )
 
      CALL TCASE ( 'Check to make sure that a pair of '
     .//           'consecutive delimiters is recognized by '
     .//           'ZZISPT. ' )
 
 
         STRING = ' 12 JAN 1992 A.D.,/ 11:12:18 P.M.'
C
C        First tokenize the string
C
         DID    = ZZTOKNS ( STRING, ERROR )
         GOT    = ZZISPT  ( ',/:', B, E  )
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',   GOT, .TRUE.,      OK )
         CALL CHCKSL ( 'DID',   DID, .TRUE.,      OK )
         CALL CHCKSC ( 'STRING(B:E)', STRING(B:E),  '=', ',/', OK )
 
 
         STRING = ' 12 JAN 1992 A.D., 11:12:18 P.M.'
C
C        First tokenize the string
C
         DID    = ZZTOKNS ( STRING, ERROR )
         GOT    = ZZISPT  ( STRING, B, E  )
         STRING = ' 12 JAN 1992 A.D., 11:12:18 P.M.'
C
C        First tokenize the string
C
         DID    = ZZTOKNS ( STRING, ERROR )
         GOT    = ZZISPT  ( ',/:', B, E  )
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT',   GOT, .FALSE.,     OK )
         CALL CHCKSL ( 'DID',   DID, .TRUE.,      OK )
         CALL CHCKSI ( 'B',     B, '=', 0, 0,     OK )
         CALL CHCKSI ( 'E',     E, '=', 0, 0,     OK )
 
      CALL TCASE ( 'Make sure that the function ZZIST works as '
     .//           'expected. ' )
 
 
         STRING = ' 12 JAN 1992 A.D., 11:12:18 P.M.'
C
C        First tokenize the string
C
         DID    = ZZTOKNS ( STRING, ERROR )
 
         CALL CHCKSL ( 'DID',   DID, .TRUE., OK )
 
         GOT    = ZZIST  ( 'i' )
         CALL CHCKSL ( 'GOT1',  GOT, .TRUE., OK )
 
         GOT    = ZZIST  ( 'm' )
         CALL CHCKSL ( 'GOT2',  GOT, .TRUE., OK )
 
         GOT    = ZZIST  ( ':' )
         CALL CHCKSL ( 'GOT3',  GOT, .TRUE., OK )
 
         GOT    = ZZIST  ( 'e' )
         CALL CHCKSL ( 'GOT4',  GOT, .TRUE., OK )
 
         GOT    = ZZIST  ( 'q' )
         CALL CHCKSL ( 'GOT5',  GOT, .FALSE., OK )
 
         GOT    = ZZIST  ( 'N' )
         CALL CHCKSL ( 'GOT6',  GOT, .TRUE.,  OK )
 
         GOT    = ZZIST  ( 'Z' )
         CALL CHCKSL ( 'GOT7',  GOT, .FALSE.,  OK )
 
 
      CALL TCASE ( 'Verify that ZZVALT replaces tokens when it '
     .//           'should and that it leaves them alone when '
     .//           'there is nothing to do. ' )
 
 
         STRING = ' 12 JAN 2012 A.D., 11:12:18 P.M.'
C
C        First tokenize the string
C
         DID    = ZZTOKNS ( STRING, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'DID',   DID, .TRUE., OK )
 
         GOT    = ZZVALT ( STRING, 1000, 1000000, 'Y' )
         DID    = ZZGREP ( REP )
         EREP   = 'bibmbYbe,bi:i:ibN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT1',  GOT, .TRUE.,    OK )
         CALL CHCKSL ( 'DID',   DID, .TRUE.,    OK )
         CALL CHCKSC ( 'REP',   REP, '=', EREP, OK )
 
         GOT    = ZZVALT ( STRING, 1000, 1000000, 'K' )
         DID    = ZZGREP ( REP )
         EREP   = 'bibmbYbe,bi:i:ibN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT2',   GOT, .FALSE.,   OK )
         CALL CHCKSL ( 'DID',   DID,  .TRUE.,    OK )
         CALL CHCKSC ( 'REP',   REP, '=', EREP,  OK )
 
      CALL TCASE ( 'Check to make sure that ZZUNPCK can unpack '
     .//           'items from a string. ' )
 
         STRING = ' 12 JAN 2012 A.D., 11:12:18 P.M.'
C
C        First tokenize the string
C
         DID    = ZZTOKNS ( STRING, ERROR )
 
         CALL CHCKSL ( 'DID',   DID, .TRUE., OK )
 
         GOT    = ZZVALT ( STRING, 1000, 1000000, 'Y' )
         DID    = ZZGREP ( REP )
         EREP   = 'bibmbYbe,bi:i:ibN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT1',  GOT, .TRUE.,    OK )
         CALL CHCKSL ( 'DID',   DID, .TRUE.,    OK )
         CALL CHCKSC ( 'REP',   REP, '=', EREP, OK )
 
 
         DID = ZZREMT ( 'b' )
         DID = ZZREMT ( ':' )
         DID = ZZREMT ( ',' )
         DID = ZZREMT ( 'e' )
         DID = ZZREMT ( 'N' )
 
         DID = ZZSUBT  ( 'imYiii', 'DmYHMS', L2R )
         GOT = ZZUNPCK (  STRING, YABBRV,
     .                    TVEC,   NTVEC, TRANSL, PIC,  ERROR )
 
         ETVEC(1) = 2012.0D0
         ETVEC(2) =    1.0D0
         ETVEC(3) =   12.0D0
         ETVEC(4) =   11.0D0
         ETVEC(5) =   12.0D0
         ETVEC(6) =   18.0D0
 
         EXPERR   = ' '
         ETRANS   = 'YMD'
         EXPN     =  6
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .TRUE.,       OK )
         CALL CHCKSL ( 'DID2',   DID,    .TRUE.,       OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',    ' ',  OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',   6, 0,  OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',   'YMD', OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC, 6, 0.0D0, OK )
 
 
         STRING = '2012 A.D. 129// 11:12:18 P.M.'
C
C        First tokenize the string
C
         DID    = ZZTOKNS ( STRING, ERROR )
 
         CALL CHCKSL ( 'DID',   DID, .TRUE., OK )
 
         GOT    = ZZVALT ( STRING, 1000, 1000000, 'Y' )
         DID    = ZZGREP ( REP )
         EREP   = 'Ybebidbi:i:ibN'
 
         CALL CHCKSL ( 'GOT1',  GOT, .TRUE.,    OK )
         CALL CHCKSL ( 'DID',   DID, .TRUE.,    OK )
         CALL CHCKSC ( 'REP',   REP, '=', EREP, OK )
 
 
         DID = ZZREMT ( 'b' )
         DID = ZZREMT ( ':' )
         DID = ZZREMT ( ',' )
         DID = ZZREMT ( 'e' )
         DID = ZZREMT ( 'd' )
         DID = ZZREMT ( 'N' )
 
         DID = ZZSUBT  ( 'Yiiii', 'YyHMS', L2R )
         GOT = ZZUNPCK (  STRING,  YABBRV,
     .                    TVEC,    NTVEC, TRANSL, PIC,  ERROR )
 
         ETVEC(1) = 2012.0D0
         ETVEC(2) =  129.0D0
         ETVEC(3) =   11.0D0
         ETVEC(4) =   12.0D0
         ETVEC(5) =   18.0D0
 
         EXPERR   = ' '
         ETRANS   = 'YMD'
         EXPN     =  6
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .TRUE.,       OK )
         CALL CHCKSL ( 'DID2',   DID,    .TRUE.,       OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '=',    ' ',  OK )
         CALL CHCKSI ( 'NTVEC',  NTVEC,  '=',   5, 0,  OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',   'YD',  OK )
         CALL CHCKAD ( 'TVEC',   TVEC,   '=',  ETVEC, 6, 0.0D0, OK )
 
      CALL TCASE ( 'Check to make sure that an unresolved string '
     .//           'is diagnosed as one by ZZUNPCK. ' )
 
 
         STRING = '2012 A.D. 129// 11:12:18 P.M.'
C
C        First tokenize the string
C
         DID    = ZZTOKNS ( STRING, ERROR )
 
         CALL CHCKSL ( 'DID',   DID, .TRUE., OK )
 
         GOT    = ZZVALT ( STRING, 1000, 1000000, 'Y' )
         DID    = ZZGREP ( REP )
         EREP   = 'Ybebidbi:i:ibN'
 
         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'GOT1',  GOT, .TRUE.,    OK )
         CALL CHCKSL ( 'DID',   DID, .TRUE.,    OK )
         CALL CHCKSC ( 'REP',   REP, '=', EREP, OK )
 
 
         DID = ZZREMT ( 'b' )
         DID = ZZREMT ( ':' )
         DID = ZZREMT ( ',' )
         DID = ZZREMT ( 'e' )
         DID = ZZREMT ( 'd' )
         DID = ZZREMT ( 'N' )
 
         GOT = ZZUNPCK (  STRING, YABBRV,
     .                    TVEC,   NTVEC, TRANSL, PIC,  ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
 
      CALL TCASE ( 'Checking Diagnostic Messages YmDHMn' )
 
         STRING = ' 1995JAN19 12 13 12'
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'imiiii', 'YmDHMn', L2R )
         GOT    =  ZZUNPCK ( STRING, YABBRV,
     .                       TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
      CALL TCASE ( 'Checking Diagnostic Messages YmYHMS' )
 
         STRING = ' 1995JAN1995 12 13 12'
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'imiiii', 'YmYHMS', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
      CALL TCASE ( 'Checking Diagnostic Messages YDHMS' )
 
         STRING = ' 1995 19 12 13 12'
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'iiiii', 'YDHMS', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
      CALL TCASE ( 'Checking Diagnostic Messages YmDyHMS' )
 
         STRING = ' 1995JAN19 95 12 13 12'
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'imiiiii', 'YmDyHMS', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
      CALL TCASE ( 'Checking Diagnostic Messages mDHMS' )
 
         STRING = 'JAN 19 12 13 12'
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'miiii', 'mDHMS', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
      CALL TCASE ( 'Checking Diagnostic Messages YmmDHMS' )
 
         STRING = '1995JAN 11 5 12 13 12'
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'imiiiii', 'YmmDHMS', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
      CALL TCASE ( 'Checking Diagnostic Messages YyyHMS' )
 
         STRING = '1995 12 11 12 13 12'
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'iiiii', 'YyyHMS', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
      CALL TCASE ( 'Checking Diagnostic Messages YDmDHMS' )
 
         STRING = '1995 12 JAN 11 12 13 12'
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'iimiiii', 'YDmDHMS', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
      CALL TCASE ( 'Checking Diagnostic Messages YDmHH' )
 
         STRING = '1995 12 JAN 11 12 '
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'iimii', 'YDmHH', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
      CALL TCASE ( 'Checking Diagnostic Messages YDmSS' )
 
         STRING = '1995 12 JAN 12 13 12'
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'iimii', 'YDmSS', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
      CALL TCASE ( 'Checking Diagnostic Messages YDmMM' )
 
         STRING = '1995 12 JAN 11 12 '
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'iimii', 'YDmMM', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
      CALL TCASE ( 'Checking Diagnostic Messages YDmMS' )
 
         STRING = '1995 12 JAN 11 12 '
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'iimii', 'YDmMS', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
 
      CALL TCASE ( 'Checking Diagnostic Messages YDmHS' )
 
 
         STRING = '1995 12 JAN 11 12 '
         DID    =  ZZTOKNS ( STRING, ERROR )
         DID    =  ZZREMT  ( 'b' )
         DID    =  ZZSUBT  ( 'iimii', 'YDmHS', L2R )
         GOT    =  ZZUNPCK (  STRING, YABBRV,
     .                        TVEC,   NTVEC, TRANSL, PIC, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',                   OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .FALSE.,      OK )
         CALL CHCKSC ( 'TRANSL', TRANSL, '=',    ' ',  OK )
         CALL CHCKSC ( 'ERROR',  ERROR,  '!=',   ' ',  OK )
 
         IF ( SHDIAG ) THEN
            CALL TSTSTY ( GOOD,   BAD    )
            CALL TSTLGS ( MSTYLE, MSTYLE )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLOG ( ERROR, .FALSE. )
            CALL TSTLOG ( ' ',   .FALSE. )
            CALL TSTLGS ( GOOD,  BAD )
         END IF
 
 
      CALL TCASE ( 'Make sure that we can perform the '
     .//           'substitution and removal of *''d tokens. ')
 
 
         STRING = 'Monday April 22, 9:24:18.19 PST 1997'
 
         DID    =  ZZTOKNS ( STRING, ERROR )
         GOT    =  ZZREMT  ( 'b' )
 
 
         CALL CHCKXC ( .FALSE., ' ',                  OK )
         CALL CHCKSL ( 'GOT1',   GOT,    .TRUE.,      OK )
         CALL CHCKSL ( 'DID1',   DID,    .TRUE.,      OK )
 
         GOT   =  ZZCMBT ( 'i.i', 'n', L2R )
         DID   =  ZZGREP ( REP )
         EREP  = 'wmi,i:i:nZi'
 
         CALL CHCKXC ( .FALSE., ' ',                  OK )
         CALL CHCKSL ( 'GOT2',   GOT,    .TRUE.,      OK )
         CALL CHCKSL ( 'DID2',   DID,    .TRUE.,      OK )
         CALL CHCKSC ( 'REP',    REP, '=', EREP,    OK )
 
         GOT  = ZZREPT ( 'i:i:n', 'H*M*S', R2L )
         DID  = ZZGREP ( REP )
         EREP = 'wmi,HMSZi'
 
         CALL CHCKXC ( .FALSE., ' ',                  OK )
         CALL CHCKSL ( 'GOT2',   GOT,      .TRUE.,    OK )
         CALL CHCKSL ( 'DID2',   DID,      .TRUE.,    OK )
         CALL CHCKSC ( 'REP',    REP, '=',  EREP,     OK )
 

      CALL TCASE ( 'Check that blank strings are properly identified.')

         EXPERR = 'The input time string is blank.'
 
         STRING = ' '
         DID    =  ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                  OK )
         CALL CHCKSL ( 'DID1',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERR1',   ERROR, '=', EXPERR,  OK )

         STRING = '  '
         DID    =  ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                  OK )
         CALL CHCKSL ( 'DID2',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERR2',   ERROR, '=', EXPERR,  OK )

         ONECHR = ' '
         DID    =  ZZTOKNS ( ONECHR, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                  OK )
         CALL CHCKSL ( 'DID3',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERR3',   ERROR, '=', EXPERR,  OK )

         DID    =  ZZTOKNS ( ' ', ERROR )

         CALL CHCKXC ( .FALSE., ' ',                  OK )
         CALL CHCKSL ( 'DID4',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERR4',   ERROR, '=', EXPERR,  OK )

         DID    =  ZZTOKNS ( '  ', ERROR )

         CALL CHCKXC ( .FALSE., ' ',                  OK )
         CALL CHCKSL ( 'DID5',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERR5',   ERROR, '=', EXPERR,  OK )



      CALL TCASE ( 'Check error detection for ASCII character 127' )

         STRING      = '1 JAN 2000'
         STRING(2:2) = CHAR(127)

         EXPERR = 'There is a non-printing, non-tab character (ASCII '
     .   //       '127) at position 2 of the time string: 1<>JAN 2000'

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL TCASE ( 'Check error detection for ASCII character 128' )

         STRING      = '1 JAN 2000'
         STRING(6:6) = CHAR(128)

         EXPERR = 'There is a non-printing, non-tab character (ASCII '
     .   //       '128) at position 6 of the time string: 1 JAN<>2000'

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )

C
C     Check ZZTOKNS token count overflow exceptions.
C     Set expected error templates to match the one in ZZTOKNS.
C
      TKNERR = 'The input time string ''#'' cannot be processed ' //
     .            'because it contains more than @ recognizable ' //
     .            'tokens. The token that could not be processed ' //
     .            'was ''#''.'
      CALL REPMI ( TKNERR, '@', MAXTKN, TKNERR )

      CALL TCASE ( 'Check that ZZTOKNS can process a string with ' //
     .             'the maximum number of tokens.' )

         STRING = ' 2'
         DO I = 4, MAXTKN, 2
            CALL SUFFIX ( '#', 1, STRING )
            CALL REPMI ( STRING, '#', I, STRING )
         END DO
         LAST = RTRIM(STRING)

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .TRUE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )


      CALL TCASE ( 'Check error detection for token count overflow, ' //
     .             'unsigned integer branch of ZZTOKNS' )

         STRING(LAST-1:) = 'pm0'
         CALL REPMC ( TKNERR, '#', STRING, EXPERR )
         CALL REPMC ( EXPERR, '#', '0',    EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL TCASE ( 'Check error detection for token count overflow, ' //
     .             'blanks branch of ZZTOKNS' )

         STRING(LAST-1:) = 'pm 0'
         CALL REPMC ( TKNERR, '#', STRING, EXPERR )
         CALL REPMC ( EXPERR, '#', ' ',    EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL TCASE ( 'Check error detection for token count overflow, ' //
     .             'TABs branch of ZZTOKNS' )

         STRING(LAST-1:) = 'pm' // CHAR(9)
         CALL REPMC ( TKNERR, '#', STRING,  EXPERR )
         CALL REPMC ( EXPERR, '#', '<TAB>', EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL TCASE ( 'Check error detection for token count overflow, ' //
     .             'other tokens branch of ZZTOKNS' )

         STRING(LAST-1:) = '0B.C.'
         CALL REPMC ( TKNERR, '#', STRING,  EXPERR )
         CALL REPMC ( EXPERR, '#', 'B.C.', EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


C
C     Check ZZTOKNS picture length overflow exceptions.
C     Set expected error template to match the one in ZZTOKNS.
C
      PICERR = 'The input time string ''#'' cannot be processed '//
     .            'because the internal picture describing it ' //
     .            'requires more than @ characters. The token that ' //
     .            'could not be processed was ''#''.'
      CALL REPMI ( PICERR, '@', PICLEN, PICERR )


      CALL TCASE ( 'Check that ZZTOKNS can process a string with ' //
     .             'the maximum possible picture length.' )

         STRING = '2007-08-09'
         STRING(PICLEN-7:) = '12:34:56'
         LAST = RTRIM(STRING)

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .TRUE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', ' ',  OK )


      CALL TCASE ( 'Check error detection for picture overflow, ' //
     .             'unsigned integer branch of ZZTOKNS' )

         STRING(LAST+1:) = '0'
         CALL REPMC ( PICERR, '#', STRING, EXPERR )
         CALL REPMC ( EXPERR, '#', '560',  EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL TCASE ( 'Check error detection for picture overflow, ' //
     .             'blanks branch of ZZTOKNS' )

         STRING(LAST+1:) = ' 0'
         CALL REPMC ( PICERR, '#', STRING, EXPERR )
         CALL REPMC ( EXPERR, '#', ' ',  EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL TCASE ( 'Check error detection for picture overflow, ' //
     .             'TABs branch of ZZTOKNS' )

         STRING(LAST+1:) = CHAR(9)
         CALL REPMC ( PICERR, '#', STRING, EXPERR )
         CALL REPMC ( EXPERR, '#', '<TAB>',  EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL TCASE ( 'Check error detection for picture overflow, ' //
     .             'month branch of ZZTOKNS' )

         STRING(LAST+1:) = 'May'
         CALL REPMC ( PICERR, '#', STRING, EXPERR )
         CALL REPMC ( EXPERR, '#', 'May',  EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL TCASE ( 'Check error detection for picture overflow, ' //
     .             'month branch of ZZTOKNS' )

         STRING(LAST+1:) = 'Sunday'
         CALL REPMC ( PICERR, '#', STRING, EXPERR )
         CALL REPMC ( EXPERR, '#', 'Sunday',  EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL TCASE ( 'Check error detection for picture overflow, ' //
     .             'era branch of ZZTOKNS' )

         STRING(LAST+1:) = 'B.C.'
         CALL REPMC ( PICERR, '#', STRING, EXPERR )
         CALL REPMC ( EXPERR, '#', 'B.C.',  EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL TCASE ( 'Check error detection for picture overflow, ' //
     .             'am/pm branch of ZZTOKNS' )

         STRING(LAST+1:) = 'AM'
         CALL REPMC ( PICERR, '#', STRING, EXPERR )
         CALL REPMC ( EXPERR, '#', 'AM',  EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL TCASE ( 'Check error detection for picture overflow, ' //
     .             'other tokens branch of ZZTOKNS' )

         STRING(LAST+1:) = '/'
         CALL REPMC ( PICERR, '#', STRING, EXPERR )
         CALL REPMC ( EXPERR, '#', '/',  EXPERR )

         DID    = ZZTOKNS ( STRING, ERROR )

         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSL ( 'DID',   DID,        .FALSE., OK )
         CALL CHCKSC ( 'ERROR', ERROR, '=', EXPERR,  OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
