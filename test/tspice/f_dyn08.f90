!$Procedure F_DYN08 ( Dynamic Frame Test Family 08 )

      SUBROUTINE F_DYN08 ( OK )

! $ Abstract
!
!     Test family to exercise the frame subsystem kernel pool
!     fetch utilities.
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

      INCLUDE 'errhnd.inc'
      INCLUDE 'zzdyn.inc'

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
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests
!
!        ZZDYNFID {fetch frame ID code}
!        ZZDYNBID {fetch body ID code}
!        ZZDYNOAC {fetch optional array, character}
!        ZZDYNOAD {fetch optional array, d.p.}
!        ZZDYNVAC {fetch array, character}
!        ZZDYNVAD {fetch array, d.p.}
!        ZZDYNVAI {fetch array, integer}
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 01-JUN-2010 (NJB)
!
!        Updated long kernel variable name error handling
!        tests to account for new checks performed in the
!        kernel pool system.
!
!-    TSPICE Version 1.0.0, 18-DEC-2004 (NJB)
!
!
!-&

!
!     SPICELIB functions


!
!     Local Parameters
!


!
!     Tolerance levels for various tests.
!

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXDEF
      PARAMETER           ( MAXDEF = 50 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 5 )

!
!     Local Variables
!
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

!
!     Saved variables
!

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_DYN08' )


! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************
!
!     ZZDYNVAD tests
!
! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAD, normal case:  fetch array using ' // &
                   'ID form of name.'                          )

!
!     First load some variables into the kernel pool.
!
      CALL CLPOOL

      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ( 1, 2, 3 )'
      DEFTXT(2) = 'FRAME_-1000000000_DP_ARRAY_2 = 4'

      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_1', &
                      MAXVAL, N,      DPVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      CALL VPACK  ( 1.D0, 2.D0, 3.D0, EXPDPV )

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )


!
!     Repeat with a second variable.
!
!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_2', &
                      MAXVAL, N,      DPVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 4.D0, 1, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAD, normal case:  fetch array using ' // &
                   'character form of name.'                    )



      DEFTXT(3) = 'FRAME_FRAME_DP_1_DP_ARRAY_1  = ( 5, 6, 7, 8 )'
      DEFTXT(4) = 'FRAME_FRAME_DP_1_DP_ARRAY_2  = 9'

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_1', &
                      MAXVAL, N,      DPVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )

      DO I = 1, 4
         EXPDPV(I) = I + 4
      END DO

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )

!
!     Repeat with a second variable.
!
!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_2', &
                      MAXVAL, N,      DPVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 9.D0, 1, 0.D0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAD, normal case:  fetch array when ' // &
                   'ID and character form of name are both '   // &
                   'present.'                                  )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_1', &
                      MAXVAL, N,      DPVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      CALL VPACK  ( 1.D0, 2.D0, 3.D0, EXPDPV )

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )


!
!     Repeat with a second variable.
!
!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_2', &
                      MAXVAL, N,      DPVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 4.D0, 1, 0.D0, OK )









!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAD, exception case:  try to fetch array ' // &
                   'having excessive size.'                         )

      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ( 1, 2, 3 )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_1', &
                      1, N,   DPVALS                )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAD, exception case:  try to fetch array ' // &
                   'having wrong data type.'                         )

      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ' // &
                  '( ''1'', ''2'', ''3'' )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNVAD ( FRAMNM, FRCODE, 'DP_ARRAY_1', &
                      3, N,   DPVALS                )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)', OK )




! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************
!
!     ZZDYNVAI tests
!
! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAI, normal case:  fetch array using ' // &
                   'ID form of name.'                          )

!
!     First load some variables into the kernel pool.
!
      CALL CLPOOL

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 2, 3 )'
      DEFTXT(2) = 'FRAME_-1000000000_IN_ARRAY_2 = 4'

      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the integer fetch routine for required arguments.
!
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1', &
                      MAXVAL, N,      INVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      DO I = 1, 3
         EXPINV(I) = I
      END DO


!
!     Check the contents of the returned array.
!
      CALL CHCKAI ( 'INVALS (1)', INVALS, '=', EXPINV, N, OK )


!
!     Repeat with a second variable.
!
!
!     Call the integer fetch routine for required arguments.
!
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_2', &
                      MAXVAL, N,      INVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKAI ( 'INVALS (2)', INVALS, '=', 4, 1, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAI, normal case:  fetch array using ' // &
                   'character form of name.'                    )



      DEFTXT(3) = 'FRAME_FRAME_IN_1_IN_ARRAY_1  = ( 5, 6, 7, 8 )'
      DEFTXT(4) = 'FRAME_FRAME_IN_1_IN_ARRAY_2  = 9'

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the integer fetch routine for required arguments.
!
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1', &
                      MAXVAL, N,      INVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )

      DO I = 1, 4
         EXPINV(I) = I + 4
      END DO

!
!     Check the contents of the returned array.
!
      CALL CHCKAI ( 'INVALS (1)', INVALS, '=', EXPINV, N, OK )

!
!     Repeat with a second variable.
!
!
!     Call the integer fetch routine for required arguments.
!
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_2', &
                      MAXVAL, N,      INVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKAI ( 'INVALS (2)', INVALS, '=', 9, 1, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAI, normal case:  fetch array when ' // &
                   'ID and character form of name are both '   // &
                   'present.'                                  )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Call the integer fetch routine for required arguments.
!
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1', &
                      MAXVAL, N,      INVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      DO I = 1, 3
         EXPINV(I) = I
      END DO

!
!     Check the contents of the returned array.
!
      CALL CHCKAI ( 'INVALS (1)', INVALS, '=', EXPINV, N, OK )


!
!     Repeat with a second variable.
!
!
!     Call the integer fetch routine for required arguments.
!
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_2', &
                      MAXVAL, N,      INVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKAI ( 'INVALS (2)', INVALS, '=', 4, 1, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAI, exception case:  try to fetch array ' // &
                   'that is not present.'                           )

!
!     Call the integer fetch routine for required arguments.
!
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_3', &
                      MAXVAL, N,      INVALS                )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAI, exception case:  try to fetch array ' // &
                   'having excessive size.'                         )

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 2, 3 )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the integer fetch routine for required arguments.
!
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1', &
                      1, N,   INVALS                )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAI, exception case:  try to fetch array ' // &
                   'having wrong data type.'                         )

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ' // &
                  '( ''1'', ''2'', ''3'' )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the integer fetch routine for required arguments.
!
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1', &
                      3, N,   INVALS                       )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAI, exception case:  try to fetch array ' // &
                   'having a value that causes integer overflow.' )

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 1.D30 )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the integer fetch routine for required arguments.
!
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1', &
                      3, N,   INVALS                       )

      CALL CHCKXC ( .TRUE., 'SPICE(INTOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAI, *non-error* exception case:  fetch ' // &
                   'array having a value that must be rounded.' )

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 2.4 )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the integer fetch routine for required arguments.
!
      CALL ZZDYNVAI ( FRAMNM, FRCODE, 'IN_ARRAY_1', &
                      3, N,   INVALS                       )


      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

      DO I = 1, N
         EXPINV(I) = I
      END DO

!
!     Check the contents of the returned array.
!
      CALL CHCKAI ( 'INVALS (1)', INVALS, '=', EXPINV, N, OK )


! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************
!
!     ZZDYNVAC tests
!
! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAC, normal case:  fetch array using ' // &
                   'ID form of name.'                          )

!
!     First load some variables into the kernel pool.
!
      CALL CLPOOL

      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ' // &
                  '( ''1'', ''2'', ''3'' )'
      DEFTXT(2) = 'FRAME_-1000000000_CH_ARRAY_2 = ''4'''

      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_1', &
                      MAXVAL, N,      CHVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      DO I = 1, N
         CALL INTSTR ( I, EXPCHV(I) )
      END DO

!
!     Check the contents of the returned array.
!
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO

!
!     Repeat with a second variable.
!
!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_2', &
                      MAXVAL, N,      CHVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '4', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAC, normal case:  fetch array using ' // &
                   'character form of name.'                    )



      DEFTXT(3) = 'FRAME_FRAME_CH_1_CH_ARRAY_1  = '// &
                  '( ''5'', ''6'', ''7'', ''8'' )'
      DEFTXT(4) = 'FRAME_FRAME_CH_1_CH_ARRAY_2  = ''9'''

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_1', &
                      MAXVAL, N,      CHVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )

      DO I = 1, N
         CALL INTSTR ( I+4, EXPCHV(I) )
      END DO

!
!     Check the contents of the returned array.
!
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO

!
!     Repeat with a second variable.
!
!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_2', &
                      MAXVAL, N,      CHVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '9', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAC, normal case:  fetch array when ' // &
                   'ID and character form of name are both '   // &
                   'present.'                                  )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_1', &
                      MAXVAL, N,      CHVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      DO I = 1, N
         CALL INTSTR ( I, EXPCHV(I) )
      END DO

!
!     Check the contents of the returned array.
!
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO

!
!     Repeat with a second variable.
!
!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_2', &
                      MAXVAL, N,      CHVALS                )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '4', OK )



! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAC, exception case:  try to fetch array ' // &
                   'that is not present.'                           )

!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_3', &
                      MAXVAL, N,      CHVALS                )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAC, exception case:  try to fetch array ' // &
                   'having excessive size.'                         )

      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ' // &
                  '( ''1'', ''2'', ''3'' )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_1', &
                      1, N,   CHVALS                )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNVAC, exception case:  try to fetch array ' // &
                   'having wrong data type.'                         )

      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ( 1, 2, 3 )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNVAC ( FRAMNM, FRCODE, 'CH_ARRAY_1', &
                      3, N,   CHVALS                       )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)', OK )







! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************
!
!     ZZDYNOAD tests
!
! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAD, normal case:  fetch array using ' // &
                   'ID form of name.'                          )

!
!     First load some variables into the kernel pool.
!
      CALL CLPOOL

      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ( 1, 2, 3 )'
      DEFTXT(2) = 'FRAME_-1000000000_DP_ARRAY_2 = 4'

      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_1', &
                      MAXVAL, N,      DPVALS, FOUND        )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      CALL VPACK  ( 1.D0, 2.D0, 3.D0, EXPDPV )

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )


!
!     Repeat with a second variable.
!
!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_2', &
                      MAXVAL, N,      DPVALS, FOUND        )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 4.D0, 1, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAD, normal case:  fetch array using ' // &
                   'character form of name.'                    )



      DEFTXT(3) = 'FRAME_FRAME_DP_1_DP_ARRAY_1  = ( 5, 6, 7, 8 )'
      DEFTXT(4) = 'FRAME_FRAME_DP_1_DP_ARRAY_2  = 9'

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_1', &
                      MAXVAL, N,      DPVALS, FOUND        )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )

      DO I = 1, 4
         EXPDPV(I) = I + 4
      END DO

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )

!
!     Repeat with a second variable.
!
!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_2', &
                      MAXVAL, N,      DPVALS, FOUND        )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 9.D0, 1, 0.D0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAD, normal case:  fetch array when ' // &
                   'ID and character form of name are both '   // &
                   'present.'                                  )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_1', &
                      MAXVAL, N,      DPVALS, FOUND        )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      CALL VPACK  ( 1.D0, 2.D0, 3.D0, EXPDPV )

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (1)', DPVALS, '=', EXPDPV, N, 0.D0, OK )


!
!     Repeat with a second variable.
!
!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_2', &
                      MAXVAL, N,      DPVALS, FOUND        )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKAD ( 'DPVALS (2)', DPVALS, '=', 4.D0, 1, 0.D0, OK )





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAD, exception case:  try to fetch array ' // &
                   'that is not present.'                           )

!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_3', &
                      MAXVAL, N,      DPVALS,  FOUND       )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAD, exception case:  try to fetch array ' // &
                   'having excessive size.'                         )

      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ( 1, 2, 3 )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_1', &
                      1, N,   DPVALS, FOUND                )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAD, exception case:  try to fetch array ' // &
                   'having wrong data type.'                         )

      FRAMNM    = 'FRAME_DP_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_DP_ARRAY_1 = ' // &
                  '( ''1'', ''2'', ''3'' )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the d.p. fetch routine for required arguments.
!
      CALL ZZDYNOAD ( FRAMNM, FRCODE, 'DP_ARRAY_1', &
                      3, N,   DPVALS, FOUND                )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)', OK )



! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************
!
!     ZZDYNOAC tests
!
! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAC, normal case:  fetch array using ' // &
                   'ID form of name.'                          )

!
!     First load some variables into the kernel pool.
!
      CALL CLPOOL

      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ' // &
                  '( ''1'', ''2'', ''3'' )'
      DEFTXT(2) = 'FRAME_-1000000000_CH_ARRAY_2 = ''4'''

      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_1', &
                      MAXVAL, N,      CHVALS, FOUND         )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      DO I = 1, N
         CALL INTSTR ( I, EXPCHV(I) )
      END DO

!
!     Check the contents of the returned array.
!
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO

!
!     Repeat with a second variable.
!
!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_2', &
                      MAXVAL, N,      CHVALS, FOUND         )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '4', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAC, normal case:  fetch array using ' // &
                   'character form of name.'                    )



      DEFTXT(3) = 'FRAME_FRAME_CH_1_CH_ARRAY_1  = '// &
                  '( ''5'', ''6'', ''7'', ''8'' )'
      DEFTXT(4) = 'FRAME_FRAME_CH_1_CH_ARRAY_2  = ''9'''

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_1', &
                      MAXVAL, N,      CHVALS, FOUND         )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 4, 0, OK )

      DO I = 1, N
         CALL INTSTR ( I+4, EXPCHV(I) )
      END DO

!
!     Check the contents of the returned array.
!
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO

!
!     Repeat with a second variable.
!
!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_2', &
                      MAXVAL, N,      CHVALS, FOUND         )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '9', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAC, normal case:  fetch array when ' // &
                   'ID and character form of name are both '   // &
                   'present.'                                  )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_1', &
                      MAXVAL, N,      CHVALS, FOUND         )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

      DO I = 1, N
         CALL INTSTR ( I, EXPCHV(I) )
      END DO

!
!     Check the contents of the returned array.
!
      DO I = 1, N
         CALL CHCKSC ( 'CHVALS (1)', CHVALS(I), '=', EXPCHV(I), OK )
      END DO

!
!     Repeat with a second variable.
!
!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_2', &
                      MAXVAL, N,      CHVALS, FOUND         )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Check the cardinality of the returned array.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the contents of the returned array.
!
      CALL CHCKSC ( 'CHVALS (2)', CHVALS(1), '=', '4', OK )



! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAC, exception case:  try to fetch array ' // &
                   'that is not present.'                           )

!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_3', &
                      MAXVAL, N,      CHVALS, FOUND         )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the found flag.
!
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAC, exception case:  try to fetch array ' // &
                   'having excessive size.'                         )

      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ' // &
                  '( ''1'', ''2'', ''3'' )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_1', &
                      1, N,   CHVALS, FOUND         )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNOAC, exception case:  try to fetch array ' // &
                   'having wrong data type.'                         )

      FRAMNM    = 'FRAME_CH_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_CH_ARRAY_1 = ( 1, 2, 3 )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the character fetch routine for required arguments.
!
      CALL ZZDYNOAC ( FRAMNM, FRCODE, 'CH_ARRAY_1', &
                      3, N,   CHVALS, FOUND                )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLETYPE)', OK )




! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************
!
!     ZZDYNBID tests
!
! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNBID, normal case:  fetch array using ' // &
                   'ID form of name.'                          )

!
!     First load some variables into the kernel pool.
!
      CALL CLPOOL

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = 199'
      DEFTXT(2) = 'FRAME_-1000000000_IN_ARRAY_2 = ''MARS BARYCENTER'''

      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the contents of the returned ID.
!
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 199, 0, OK )


!
!     Repeat with a second variable.
!
!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the contents of the returned ID.
!
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', 4, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNBID, normal case:  fetch array using ' // &
                   'character form of name.'                    )



      DEFTXT(3) = 'FRAME_FRAME_IN_1_IN_ARRAY_1  = ''5'''
      DEFTXT(4) = 'FRAME_FRAME_IN_1_IN_ARRAY_2  = ''Pluto'''

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the contents of the returned ID.
!
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 5, 0, OK )

!
!     Repeat with a second variable.
!
!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the contents of the returned ID.
!
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', 999, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNBID, normal case:  fetch array when ' // &
                   'ID and character form of name are both '   // &
                   'present.'                                  )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = 199'
      DEFTXT(2) = 'FRAME_-1000000000_IN_ARRAY_2 = ''MARS BARYCENTER'''
      DEFTXT(3) = 'FRAME_FRAME_IN_1_IN_ARRAY_1  = ''5'''
      DEFTXT(4) = 'FRAME_FRAME_IN_1_IN_ARRAY_2  = ''Pluto'''

      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the contents of the returned ID.
!
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 199, 0, OK )


!
!     Repeat with a second variable.
!
!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the contents of the returned ID.
!
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', 4, 0, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNBID, exception case:  try to fetch array ' // &
                   'that is not present.'                           )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_3', INVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNBID, exception case:  try to fetch ' // &
                   'integer array having size > 1.' )

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 2, 3 )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNBID, exception case:  try to fetch ' // &
                   'character array having size > 1.' )

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( ''1'', ''2'' )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNBID, exception case:  try to fetch ' // &
                   'body name with no matching ID code.' )

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 =  ''PLANET X'''

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNBID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRANSLATION)', OK )





! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************
!
!     ZZDYNFID tests
!
! ****************************************************************
! ****************************************************************
! ****************************************************************
! ****************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNFID, normal case:  fetch array using ' // &
                   'ID form of name.'                          )

!
!     First load some variables into the kernel pool.
!
      CALL CLPOOL

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = 1'
      DEFTXT(2) = 'FRAME_-1000000000_IN_ARRAY_2 = ''IAU_MARS'''

      CALL LMPOOL ( DEFTXT, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the contents of the returned ID.
!
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 1, 0, OK )


!
!     Repeat with a second variable.
!
!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the contents of the returned ID.
!
      CALL NAMFRM ( 'IAU_MARS',   I )
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', I, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNFID, normal case:  fetch array using ' // &
                   'character form of name.'                    )



      DEFTXT(3) = 'FRAME_FRAME_IN_1_IN_ARRAY_1  = ''J2000'''
      DEFTXT(4) = 'FRAME_FRAME_IN_1_IN_ARRAY_2  = ''B1950'''

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( DEFTXT(3), 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the contents of the returned ID.
!
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 1, 0, OK )

!
!     Repeat with a second variable.
!
!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the contents of the returned ID.
!
      CALL NAMFRM ( 'B1950',   I )
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', I, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNFID, normal case:  fetch array when ' // &
                   'ID and character form of name are both '   // &
                   'present.'                                  )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = 1'
      DEFTXT(2) = 'FRAME_-1000000000_IN_ARRAY_2 = ''IAU_MARS'''
      DEFTXT(3) = 'FRAME_FRAME_IN_1_IN_ARRAY_1  = ''J2000'''
      DEFTXT(4) = 'FRAME_FRAME_IN_1_IN_ARRAY_2  = ''B1950'''

      CALL LMPOOL ( DEFTXT, 4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the contents of the returned ID.
!
      CALL CHCKSI ( 'INVALS (1)', INVALS, '=', 1, 0, OK )


!
!     Repeat with a second variable.
!
!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_2', INVALS )

      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check the contents of the returned ID.
!
      CALL NAMFRM ( 'IAU_MARS',   I )
      CALL CHCKSI ( 'INVALS (2)', INVALS, '=', I, 0, OK )






!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNFID, exception case:  try to fetch array ' // &
                   'that is not present.'                           )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_3', INVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNFID, exception case:  try to fetch ' // &
                   'integer array having size > 1.' )

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( 1, 2, 3 )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNFID, exception case:  try to fetch ' // &
                   'character array having size > 1.' )

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 = ( ''1'', ''2'' )'

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(BADVARIABLESIZE)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZDYNFID, exception case:  try to fetch ' // &
                   'frame name with no matching ID code.' )

      FRAMNM    = 'FRAME_IN_1'
      FRCODE    = -1000000000

      DEFTXT(1) = 'FRAME_-1000000000_IN_ARRAY_1 =  ''PLANET X'''

      CALL CLPOOL

      CALL LMPOOL ( DEFTXT, 1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Call the body ID fetch routine for required arguments.
!
      CALL ZZDYNFID ( FRAMNM, FRCODE, 'IN_ARRAY_1', INVALS )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRANSLATION)', OK )







!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
