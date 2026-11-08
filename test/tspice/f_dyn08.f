C$Procedure F_DYN08 ( Dynamic Frame Test Family 08 )
 
      SUBROUTINE F_DYN08 ( OK )
 
C$ Abstract
C
C     Test family to exercise the frame subsystem kernel pool
C     fetch utilities.
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
 
      INCLUDE 'errhnd.inc'
      INCLUDE 'zzdyn.inc'
 
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
C     This routine tests
C
C        ZZDYNFID {fetch frame ID code}
C        ZZDYNBID {fetch body ID code}
C        ZZDYNOAC {fetch optional array, character}
C        ZZDYNOAD {fetch optional array, d.p.}
C        ZZDYNVAC {fetch array, character}
C        ZZDYNVAD {fetch array, d.p.}
C        ZZDYNVAI {fetch array, integer}
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
C
C$ Version
C
C-    TSPICE Version 2.0.0, 01-JUN-2010 (NJB)
C
C        Updated long kernel variable name error handling
C        tests to account for new checks performed in the
C        kernel pool system.
C
C-    TSPICE Version 1.0.0, 18-DEC-2004 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
 
 
C
C     Local Parameters
C
 
 
C
C     Tolerance levels for various tests.
C
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               MAXDEF
      PARAMETER           ( MAXDEF = 50 )
 
      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 5 )
 
C
C     Local Variables
C
      CHARACTER*(LNSIZE)    CHVALS ( MAXVAL )
      CHARACTER*(LNSIZE)    DEFTXT ( MAXDEF )
      CHARACTER*(LNSIZE)    EXPCHV ( MAXVAL )
      CHARACTER*(FRNMLN)    FRAMNM
 
      DOUBLE PRECISION      EXPDPV ( MAXVAL )
      DOUBLE PRECISION      DPVALS ( MAXVAL )
 
      INTEGER               EXPINV ( MAXVAL )
      INTEGER               FRCODE
      INTEGER               I
      INTEGER               INVALS ( MAXVAL )
      INTEGER               N
 
      LOGICAL               FOUND
 
C
C     Saved variables
C
 
C
C     Initial values
C
 
C
C     Open the test family.
C
      CALL TOPEN ( 'F_DYN08' )
 
 
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
C
C     ZZDYNVAD tests
C
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAD, normal case:  fetch array using ' //
     .             'ID form of name.'                          )
 
C
C     First load some variables into the kernel pool.
C
      CALL CLPOOL
 
      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ( 1, 2, 3 )'
      DEFTXT(2) = 'FRAME_-1000000000_DP_ARRAY_2 = 4'
 
      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_1',
     .                MAXVAL, N,      DPVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
 
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, EXPDPV )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )
 
 
C
C     Repeat with a second variable.
C
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_2',
     .                MAXVAL, N,      DPVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 4.D0, 1, 0.D0, OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAD, normal case:  fetch array using ' //
     .             'character form of name.'                    )
 
 
 
      DEFTXT(3) = 'FRAME_FRAME_DP_1_DP_ARRAY_1  = ( 5, 6, 7, 8 )'
      DEFTXT(4) = 'FRAME_FRAME_DP_1_DP_ARRAY_2  = 9'
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_1',
     .                MAXVAL, N,      DPVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
 
      DO I = 1, 4
         EXPDPV(I) = I + 4
      END DO
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )
 
C
C     Repeat with a second variable.
C
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_2',
     .                MAXVAL, N,      DPVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 9.D0, 1, 0.D0, OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAD, normal case:  fetch array when ' //
     .             'ID and character form of name are both '   //
     .             'present.'                                  )
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_1',
     .                MAXVAL, N,      DPVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
 
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, EXPDPV )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )
 
 
C
C     Repeat with a second variable.
C
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_2',
     .                MAXVAL, N,      DPVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 4.D0, 1, 0.D0, OK )
 
 
 
 
 
 
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAD, exception case:  try to fetch array ' //
     .             'having excessive size.'                         )
 
      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ( 1, 2, 3 )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_1',
     .                1, N,   DPVALS                )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAD, exception case:  try to fetch array ' //
     .             'having wrong data type.'                         )
 
      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ' //
     .            '( ''1'', ''2'', ''3'' )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_1',
     .                3, N,   DPVALS                )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)', OK )
 
 
 
 
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
C
C     ZZDYNVAI tests
C
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAI, normal case:  fetch array using ' //
     .             'ID form of name.'                          )
 
C
C     First load some variables into the kernel pool.
C
      CALL CLPOOL
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 2, 3 )'
      DEFTXT(2) = 'FRAME_-1000000000_IN_ARRAY_2 = 4'
 
      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the integer fetch routine for required arguments.
C
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1',
     .                MAXVAL, N,      INVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
 
      DO I = 1, 3
         EXPINV(I) = I
      END DO
 
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAI ( 'INVALS (1)', INVALS, '=', EXPINV, N, OK )
 
 
C
C     Repeat with a second variable.
C
C
C     Call the integer fetch routine for required arguments.
C
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_2',
     .                MAXVAL, N,      INVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAI ( 'INVALS (2)', INVALS, '=', 4, 1, OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAI, normal case:  fetch array using ' //
     .             'character form of name.'                    )
 
 
 
      DEFTXT(3) = 'FRAME_FRAME_IN_1_IN_ARRAY_1  = ( 5, 6, 7, 8 )'
      DEFTXT(4) = 'FRAME_FRAME_IN_1_IN_ARRAY_2  = 9'
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the integer fetch routine for required arguments.
C
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1',
     .                MAXVAL, N,      INVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
 
      DO I = 1, 4
         EXPINV(I) = I + 4
      END DO
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAI ( 'INVALS (1)', INVALS, '=', EXPINV, N, OK )
 
C
C     Repeat with a second variable.
C
C
C     Call the integer fetch routine for required arguments.
C
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_2',
     .                MAXVAL, N,      INVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAI ( 'INVALS (2)', INVALS, '=', 9, 1, OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAI, normal case:  fetch array when ' //
     .             'ID and character form of name are both '   //
     .             'present.'                                  )
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Call the integer fetch routine for required arguments.
C
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1',
     .                MAXVAL, N,      INVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
 
      DO I = 1, 3
         EXPINV(I) = I
      END DO
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAI ( 'INVALS (1)', INVALS, '=', EXPINV, N, OK )
 
 
C
C     Repeat with a second variable.
C
C
C     Call the integer fetch routine for required arguments.
C
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_2',
     .                MAXVAL, N,      INVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAI ( 'INVALS (2)', INVALS, '=', 4, 1, OK )
 
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAI, exception case:  try to fetch array ' //
     .             'that is not present.'                           )
 
C
C     Call the integer fetch routine for required arguments.
C
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_3',
     .                MAXVAL, N,      INVALS                )
 
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAI, exception case:  try to fetch array ' //
     .             'having excessive size.'                         )
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 2, 3 )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the integer fetch routine for required arguments.
C
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1',
     .                1, N,   INVALS                )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAI, exception case:  try to fetch array ' //
     .             'having wrong data type.'                         )
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ' //
     .            '( ''1'', ''2'', ''3'' )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the integer fetch routine for required arguments.
C
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1',
     .                3, N,   INVALS                       )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)', OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAI, exception case:  try to fetch array ' //
     .             'having a value that causes integer overflow.' )
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 1.D30 )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the integer fetch routine for required arguments.
C
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1',
     .                3, N,   INVALS                       )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INTOUTOFRANGE)', OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAI, *non-error* exception case:  fetch ' //
     .             'array having a value that must be rounded.' )
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 2.4 )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the integer fetch routine for required arguments.
C
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1',
     .                3, N,   INVALS                       )
 
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )
 
      DO I = 1, N
         EXPINV(I) = I
      END DO
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAI ( 'INVALS (1)', INVALS, '=', EXPINV, N, OK )
 
 
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
C
C     ZZDYNVAC tests
C
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAC, normal case:  fetch array using ' //
     .             'ID form of name.'                          )
 
C
C     First load some variables into the kernel pool.
C
      CALL CLPOOL
 
      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ' //
     .            '( ''1'', ''2'', ''3'' )'
      DEFTXT(2) = 'FRAME_-1000000000_CH_ARRAY_2 = ''4'''
 
      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_1',
     .                MAXVAL, N,      CHVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
 
      DO I = 1, N
         CALL INTSTR ( I, EXPCHV(I) )
      END DO
 
C
C     Check the contents of the returned array.
C
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO
 
C
C     Repeat with a second variable.
C
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_2',
     .                MAXVAL, N,      CHVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '4', OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAC, normal case:  fetch array using ' //
     .             'character form of name.'                    )
 
 
 
      DEFTXT(3) = 'FRAME_FRAME_CH_1_CH_ARRAY_1  = '//
     .            '( ''5'', ''6'', ''7'', ''8'' )'
      DEFTXT(4) = 'FRAME_FRAME_CH_1_CH_ARRAY_2  = ''9'''
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_1',
     .                MAXVAL, N,      CHVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
 
      DO I = 1, N
         CALL INTSTR ( I+4, EXPCHV(I) )
      END DO
 
C
C     Check the contents of the returned array.
C
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO
 
C
C     Repeat with a second variable.
C
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_2',
     .                MAXVAL, N,      CHVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '9', OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAC, normal case:  fetch array when ' //
     .             'ID and character form of name are both '   //
     .             'present.'                                  )
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_1',
     .                MAXVAL, N,      CHVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
 
      DO I = 1, N
         CALL INTSTR ( I, EXPCHV(I) )
      END DO
 
C
C     Check the contents of the returned array.
C
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO
 
C
C     Repeat with a second variable.
C
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_2',
     .                MAXVAL, N,      CHVALS                )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '4', OK )
 
 
 
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAC, exception case:  try to fetch array ' //
     .             'that is not present.'                           )
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_3',
     .                MAXVAL, N,      CHVALS                )
 
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAC, exception case:  try to fetch array ' //
     .             'having excessive size.'                         )
 
      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ' //
     .            '( ''1'', ''2'', ''3'' )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_1',
     .                1, N,   CHVALS                )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNVAC, exception case:  try to fetch array ' //
     .             'having wrong data type.'                         )
 
      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ( 1, 2, 3 )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_1',
     .                3, N,   CHVALS                       )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)', OK )
 
 
 
 
 
 
 
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
C
C     ZZDYNOAD tests
C
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAD, normal case:  fetch array using ' //
     .             'ID form of name.'                          )
 
C
C     First load some variables into the kernel pool.
C
      CALL CLPOOL
 
      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ( 1, 2, 3 )'
      DEFTXT(2) = 'FRAME_-1000000000_DP_ARRAY_2 = 4'
 
      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_1',
     .                MAXVAL, N,      DPVALS, FOUND        )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
 
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, EXPDPV )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )
 
 
C
C     Repeat with a second variable.
C
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_2',
     .                MAXVAL, N,      DPVALS, FOUND        )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 4.D0, 1, 0.D0, OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAD, normal case:  fetch array using ' //
     .             'character form of name.'                    )
 
 
 
      DEFTXT(3) = 'FRAME_FRAME_DP_1_DP_ARRAY_1  = ( 5, 6, 7, 8 )'
      DEFTXT(4) = 'FRAME_FRAME_DP_1_DP_ARRAY_2  = 9'
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_1',
     .                MAXVAL, N,      DPVALS, FOUND        )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
 
      DO I = 1, 4
         EXPDPV(I) = I + 4
      END DO
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )
 
C
C     Repeat with a second variable.
C
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_2',
     .                MAXVAL, N,      DPVALS, FOUND        )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 9.D0, 1, 0.D0, OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAD, normal case:  fetch array when ' //
     .             'ID and character form of name are both '   //
     .             'present.'                                  )
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_1',
     .                MAXVAL, N,      DPVALS, FOUND        )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
 
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, EXPDPV )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )
 
 
C
C     Repeat with a second variable.
C
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_2',
     .                MAXVAL, N,      DPVALS, FOUND        )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 4.D0, 1, 0.D0, OK )
 
 
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAD, exception case:  try to fetch array ' //
     .             'that is not present.'                           )
 
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_3',
     .                MAXVAL, N,      DPVALS,  FOUND       )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAD, exception case:  try to fetch array ' //
     .             'having excessive size.'                         )
 
      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ( 1, 2, 3 )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_1',
     .                1, N,   DPVALS, FOUND                )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAD, exception case:  try to fetch array ' //
     .             'having wrong data type.'                         )
 
      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ' //
     .            '( ''1'', ''2'', ''3'' )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the d.p. fetch routine for required arguments.
C
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_1',
     .                3, N,   DPVALS, FOUND                )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)', OK )
 
 
 
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
C
C     ZZDYNOAC tests
C
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAC, normal case:  fetch array using ' //
     .             'ID form of name.'                          )
 
C
C     First load some variables into the kernel pool.
C
      CALL CLPOOL
 
      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ' //
     .            '( ''1'', ''2'', ''3'' )'
      DEFTXT(2) = 'FRAME_-1000000000_CH_ARRAY_2 = ''4'''
 
      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_1',
     .                MAXVAL, N,      CHVALS, FOUND         )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
 
      DO I = 1, N
         CALL INTSTR ( I, EXPCHV(I) )
      END DO
 
C
C     Check the contents of the returned array.
C
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO
 
C
C     Repeat with a second variable.
C
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_2',
     .                MAXVAL, N,      CHVALS, FOUND         )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '4', OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAC, normal case:  fetch array using ' //
     .             'character form of name.'                    )
 
 
 
      DEFTXT(3) = 'FRAME_FRAME_CH_1_CH_ARRAY_1  = '//
     .            '( ''5'', ''6'', ''7'', ''8'' )'
      DEFTXT(4) = 'FRAME_FRAME_CH_1_CH_ARRAY_2  = ''9'''
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_1',
     .                MAXVAL, N,      CHVALS, FOUND         )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )
 
      DO I = 1, N
         CALL INTSTR ( I+4, EXPCHV(I) )
      END DO
 
C
C     Check the contents of the returned array.
C
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO
 
C
C     Repeat with a second variable.
C
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_2',
     .                MAXVAL, N,      CHVALS, FOUND         )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '9', OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAC, normal case:  fetch array when ' //
     .             'ID and character form of name are both '   //
     .             'present.'                                  )
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_1',
     .                MAXVAL, N,      CHVALS, FOUND         )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )
 
      DO I = 1, N
         CALL INTSTR ( I, EXPCHV(I) )
      END DO
 
C
C     Check the contents of the returned array.
C
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO
 
C
C     Repeat with a second variable.
C
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_2',
     .                MAXVAL, N,      CHVALS, FOUND         )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Check the cardinality of the returned array.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )
 
C
C     Check the contents of the returned array.
C
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '4', OK )
 
 
 
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAC, exception case:  try to fetch array ' //
     .             'that is not present.'                           )
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_3',
     .                MAXVAL, N,      CHVALS, FOUND         )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the found flag.
C
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAC, exception case:  try to fetch array ' //
     .             'having excessive size.'                         )
 
      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ' //
     .            '( ''1'', ''2'', ''3'' )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_1',
     .                1, N,   CHVALS, FOUND         )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNOAC, exception case:  try to fetch array ' //
     .             'having wrong data type.'                         )
 
      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ( 1, 2, 3 )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the character fetch routine for required arguments.
C
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_1',
     .                3, N,   CHVALS, FOUND                )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)', OK )
 
 
 
 
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
C
C     ZZDYNBID tests
C
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNBID, normal case:  fetch array using ' //
     .             'ID form of name.'                          )
 
C
C     First load some variables into the kernel pool.
C
      CALL CLPOOL
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = 199'
      DEFTXT(2) = 'FRAME_-1000000000_IN_ARRAY_2 = ''MARS BARYCENTER'''
 
      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Check the contents of the returned ID.
C
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 199, 0, OK )
 
 
C
C     Repeat with a second variable.
C
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the contents of the returned ID.
C
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', 4, 0, OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNBID, normal case:  fetch array using ' //
     .             'character form of name.'                    )
 
 
 
      DEFTXT(3) = 'FRAME_FRAME_IN_1_IN_ARRAY_1  = ''5'''
      DEFTXT(4) = 'FRAME_FRAME_IN_1_IN_ARRAY_2  = ''Pluto'''
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Check the contents of the returned ID.
C
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 5, 0, OK )
 
C
C     Repeat with a second variable.
C
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Check the contents of the returned ID.
C
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', 999, 0, OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNBID, normal case:  fetch array when ' //
     .             'ID and character form of name are both '   //
     .             'present.'                                  )
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = 199'
      DEFTXT(2) = 'FRAME_-1000000000_IN_ARRAY_2 = ''MARS BARYCENTER'''
      DEFTXT(3) = 'FRAME_FRAME_IN_1_IN_ARRAY_1  = ''5'''
      DEFTXT(4) = 'FRAME_FRAME_IN_1_IN_ARRAY_2  = ''Pluto'''
 
      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Check the contents of the returned ID.
C
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 199, 0, OK )
 
 
C
C     Repeat with a second variable.
C
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Check the contents of the returned ID.
C
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', 4, 0, OK )
 
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNBID, exception case:  try to fetch array ' //
     .             'that is not present.'                           )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_3', INVALS )
 
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNBID, exception case:  try to fetch ' //
     .             'integer array having size > 1.' )
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 2, 3 )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNBID, exception case:  try to fetch ' //
     .             'character array having size > 1.' )
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( ''1'', ''2'' )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNBID, exception case:  try to fetch ' //
     .             'body name with no matching ID code.' )
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 =  ''PLANET X'''
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRANSLATION)', OK )
 
 
 
 
 
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
C
C     ZZDYNFID tests
C
C ****************************************************************
C ****************************************************************
C ****************************************************************
C ****************************************************************
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNFID, normal case:  fetch array using ' //
     .             'ID form of name.'                          )
 
C
C     First load some variables into the kernel pool.
C
      CALL CLPOOL
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = 1'
      DEFTXT(2) = 'FRAME_-1000000000_IN_ARRAY_2 = ''IAU_MARS'''
 
      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Check the contents of the returned ID.
C
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 1, 0, OK )
 
 
C
C     Repeat with a second variable.
C
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the contents of the returned ID.
C
      CALL NAMFRM ( 'IAU_MARS',   I )
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', I, 0, OK )
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNFID, normal case:  fetch array using ' //
     .             'character form of name.'                    )
 
 
 
      DEFTXT(3) = 'FRAME_FRAME_IN_1_IN_ARRAY_1  = ''J2000'''
      DEFTXT(4) = 'FRAME_FRAME_IN_1_IN_ARRAY_2  = ''B1950'''
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Check the contents of the returned ID.
C
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 1, 0, OK )
 
C
C     Repeat with a second variable.
C
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Check the contents of the returned ID.
C
      CALL NAMFRM ( 'B1950',   I )
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', I, 0, OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNFID, normal case:  fetch array when ' //
     .             'ID and character form of name are both '   //
     .             'present.'                                  )
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = 1'
      DEFTXT(2) = 'FRAME_-1000000000_IN_ARRAY_2 = ''IAU_MARS'''
      DEFTXT(3) = 'FRAME_FRAME_IN_1_IN_ARRAY_1  = ''J2000'''
      DEFTXT(4) = 'FRAME_FRAME_IN_1_IN_ARRAY_2  = ''B1950'''
 
      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Check the contents of the returned ID.
C
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 1, 0, OK )
 
 
C
C     Repeat with a second variable.
C
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Check the contents of the returned ID.
C
      CALL NAMFRM ( 'IAU_MARS',   I )
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', I, 0, OK )
 
 
 
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNFID, exception case:  try to fetch array ' //
     .             'that is not present.'                           )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_3', INVALS )
 
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNFID, exception case:  try to fetch ' //
     .             'integer array having size > 1.' )
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 2, 3 )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNFID, exception case:  try to fetch ' //
     .             'character array having size > 1.' )
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( ''1'', ''2'' )'
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )
 
 
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZDYNFID, exception case:  try to fetch ' //
     .             'frame name with no matching ID code.' )
 
      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000
 
      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 =  ''PLANET X'''
 
      CALL CLPOOL
 
      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Call the body ID fetch routine for required arguments.
C
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )
 
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRANSLATION)', OK )
 
 
 
 
 
 
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
