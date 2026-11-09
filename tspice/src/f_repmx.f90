!$Procedure F_REPMX ( Family of tests for replace marker routines )

      SUBROUTINE F_REPMX ( OK )

! $ Abstract
!
!     Perform a series of tests on the REPMC, REPMCT, REPMD,
!     REPMF, REPMI, REPML and REPMOT routines.
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

      LOGICAL  OK

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
!     This routine tests a family of related routines for inserting
!     values into strings:
!
!        REPMC
!        REPMCT
!        REPMD
!        REPMF
!        REPMI
!        REPML
!        REPMOT
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     M. Liukis          (JPL)
!     B.V. Semenov       (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 20-JAN-2021 (NJB) (JDR) (BVS) (ML)
!
!        Updated header with missing sections. Added test cases
!        for the REPMCT, REPML, REPMOT routines.
!
!-    TSPICE Version 1.0.0, 30-AUG-1990 (NJB) (IMU)
!
!-&


! $ Index_Entries
!
!     Test family for the replace marker routines
!
!-&


!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(LNSIZE)    RESULT
      CHARACTER*(LNSIZE)    EXPLIN

      CHARACTER*(WDSIZE)    WORD
      CHARACTER*(WDSIZE)    WEXP


      CHARACTER*(WDSIZE)    SUB
      INTEGER               I
      DOUBLE PRECISION      X

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_REPMX' )

!
!     *****************************************************************
!
!     Normal cases: REPMC
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Character substitution  -- check REPMC' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''ok''.'
      SUB    = 'ok'

      CALL REPMC ( LINE, '#', SUB, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Character substitution in-place -- check REPMC' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''ok''.'
      SUB    = 'ok'

      CALL REPMC ( RESULT, '#', SUB, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Character substitution truncated -- check REPMC' )

      LINE    = 'The value is ''#''.'
      EXPLIN  = 'The value is ''a very long string indeed''.'
      SUB     = 'a very long string indeed'

      CALL REPMC ( LINE, '#', SUB, WORD )

      WEXP = EXPLIN(:WDSIZE)

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Character substitution in-place truncated ' &
      //           '-- check REPMC' )

      WORD   = 'The value is ''#''.'
      EXPLIN = 'The value is ''a very long string indeed''.'
      SUB    = 'a very long string indeed'

      CALL REPMC ( WORD, '#', SUB, WORD )

      WEXP = EXPLIN(:WDSIZE)

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )

!
!     *****************************************************************
!
!     Normal cases: REPMI
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Integer substitution -- check REPMI' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''3''.'
      I      = 3

      CALL REPMI ( LINE, '#', I, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Integer substitution in-place -- check REPMI' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''3''.'
      I      = 3

      CALL REPMI ( RESULT, '#', I, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     *****************************************************************
!
!     Normal cases: REPMD
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('Double precision substitution (scientific) -- REPMD')

      LINE   = 'The value is ''#''.'
      EXPLIN ='The value is ''4.0E+00''.'
      X      = 4.D0

      CALL REPMD ( LINE, '#', X, 2, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Double precision substitution in-place ' &
      //           '(scientific) -- REPMD' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''4.0E+00''.'
      X      = 4.D0

      CALL REPMD ( LINE, '#', X, 2, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     *****************************************************************
!
!     Normal cases: REPMF
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('Double precision substitution (float) -- REPMF')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''4.0''.'
      X      = 4.D0

      CALL REPMF ( LINE, '#', X, 2, 'f',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Double precision substitution in-place ' &
      //           '(float) -- REPMF' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''4.0''.'
      X      = 4.D0

      CALL REPMF ( RESULT, '#', X, 2, 'g',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     *****************************************************************
!
!     Normal cases: REPML
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('Logical value substitution (empty marker) ' &
      //          '-- check REPML')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'

      CALL REPML ( LINE, ' ', .TRUE., 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Logical value substitution ' &
      //           '(non-existent marker) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'

      CALL REPML ( LINE, 'abc', .TRUE., 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Logical value substitution ' &
      //           '(first marker, TRUE) -- check REPML' )

      LINE   = 'The values are ''#'' and ''#''.'
      EXPLIN = 'The values are ''TRUE'' and ''#''.'

      CALL REPML ( LINE, '#', .TRUE., 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Logical value substitution truncated ' &
      //           '-- check REPML' )

      LINE   = 'The values are long ''#'' and ''#''.'
      EXPLIN = 'The values are long ''TRUE'' and ''#''.'
      WEXP   = EXPLIN( 1:WDSIZE )

      CALL REPML ( LINE, '#', .TRUE., 'u',  WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Logical value substitution in-place ' &
      //           '(TRUE) -- check REPML' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''TRUE''.'

      CALL REPML ( RESULT, '#', .TRUE., 'U',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Logical value substitution in-place ' &
      //           ' truncated -- check REPML' )

      WORD   = 'The value is ''#'' and ''#''.'
      EXPLIN = 'The value is ''TRUE'' and ''#''.'
      WEXP   = EXPLIN( 1:WDSIZE )

      CALL REPML ( WORD, '#', .TRUE., 'U',  WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Logical value substitution ' &
      //           '(True) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''True''.'

      CALL REPML ( LINE, '#', .TRUE., 'c',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Logical value substitution ' &
      //           '(true) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''true''.'

      CALL REPML ( LINE, '#', .TRUE., 'l',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Logical value substitution ' &
      //           '(FALSE) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''FALSE''.'

      CALL REPML ( LINE, '#', .FALSE., 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Logical value substitution ' &
      //           '(False) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''False''.'

      CALL REPML ( LINE, '#', .FALSE., 'c',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Logical value substitution ' &
      //           '(false) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''false''.'

      CALL REPML ( LINE, '#', .FALSE., 'l',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     *****************************************************************
!
!     Normal cases: REPMCT
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('Cardinal text substitution (empty marker) ' &
      //          '-- check REPMCT')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'
      I      = 55

      CALL REPMCT ( LINE, ' ', I, 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('Cardinal text substitution ' &
      //          '(non-existent marker) -- check REPMCT')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'
      I      = 55

      CALL REPMCT ( LINE, 'abc', I, 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Cardinal text substitution, lowercase ' &
      //           '-- check REPMCT' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''fifty-five''.'
      I      = 55

      CALL REPMCT ( LINE, '#', I, 'l', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Cardinal text substitution, uppercase ' &
      //           '-- check REPMCT' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''FIFTY-FIVE''.'
      I      = 55

      CALL REPMCT ( LINE, '#', I, 'U', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Cardinal text substitution, capitalized ' &
      //           '-- check REPMCT' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''Fifty-five''.'
      I      = 55

      CALL REPMCT ( LINE, '#', I, 'c', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Cardinal text substitution truncated ' &
      //           '-- check REPMCT' )

      LINE    = 'The value is ''#''.'
      EXPLIN  = 'The value is ''seven hundred seven''.'
      WEXP    = EXPLIN( 1:WDSIZE )

      I       = 777777777

      CALL REPMCT ( LINE, '#', I, 'l', WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Cardinal text substitution in-place ' &
      //           '-- check REPMCT' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''fifty-five''.'
      I      = 55

      CALL REPMCT ( RESULT, '#', I, 'L', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Cardinal text substitution in-place truncated ' &
      //           '-- check REPMCT' )

      WORD   = 'The value is ''#''.'
      EXPLIN = 'The value is ''seven hundred seven''.'
      WEXP   = EXPLIN( 1:WDSIZE )

      I      = 777777777

      CALL REPMCT ( WORD, '#', I, 'l', WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


!
!     *****************************************************************
!
!     Normal cases: REPMOT
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('Ordinal text substitution (empty marker) ' &
      //          '-- check REPMOT')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'
      I      = 55

      CALL REPMOT ( LINE, ' ', I, 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('Ordinal text substitution ' &
      //          '(non-existent marker) -- check REPMOT')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'
      I      = 55

      CALL REPMOT ( LINE, 'abc', I, 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Ordinal text substitution, lowercase ' &
      //           '-- check REPMOT' )

      LINE   = 'The ''#'' value is 55.'
      EXPLIN = 'The ''fifty-fifth'' value is 55.'
      I      = 55

      CALL REPMOT ( LINE, '#', I, 'l', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Ordinal text substitution, uppercase ' &
      //           '-- check REPMOT' )

      LINE   = 'The ''#'' value is 55.'
      EXPLIN = 'The ''FIFTY-FIFTH'' value is 55.'
      I      = 55

      CALL REPMOT ( LINE, '#', I, 'U', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Ordinal text substitution, capitalized ' &
      //           '-- check REPMOT' )

      LINE   = 'The ''#'' value is 55.'
      EXPLIN = 'The ''Fifty-fifth'' value is 55.'
      I      = 55

      CALL REPMOT ( LINE, '#', I, 'c', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Ordinal text substitution truncated ' &
      //           '-- check REPMOT' )

      LINE   = 'The ''#'' value is 777 777 777.'
      EXPLIN = 'The ''seven hundred seventy-seven million''.'
      WEXP   = EXPLIN( 1:WDSIZE )

      I      = 777777777

      CALL REPMOT ( LINE, '#', I, 'l', WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Ordinal text substitution in-place ' &
      //           '-- check REPMOT' )

      RESULT = 'The ''#'' value is 77.'
      EXPLIN = 'The ''seventy-seventh'' value is 77.'
      I      = 77

      CALL REPMOT ( RESULT, '#', I, 'L', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Ordinal text substitution in-place truncated ' &
      //           '-- check REPMOT' )

      WORD   = 'The ''#'' value is 77.'
      EXPLIN = 'The ''seventy-seventh'' value is 77.'
      WEXP   = EXPLIN( 1:WDSIZE )

      I      = 77

      CALL REPMOT ( WORD, '#', I, 'l', WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


!
!     *****************************************************************
!
!     Error cases: REPMCT
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Cardinal text substitution ' &
      //           '(invalid case flag) -- check REPMCT' )

      LINE = 'The value is ''#''.'
      I    = 55

      CALL REPMCT ( LINE, '#', I, 'n',  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCASE)',  OK )


!
!     *****************************************************************
!
!     Error cases: REPML
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Logical value substitution ' &
      //           '(invalid case flag) -- check REPML' )

      LINE = 'The value is ''#''.'

      CALL REPML  ( LINE, '#', .TRUE., 'n',  RESULT   )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCASE)',  OK )

!
!     *****************************************************************
!
!     Error cases: REPMOT
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Ordinal text substitution ' &
      //           '(invalid case flag) -- check REPMOT' )

      LINE = 'The value is ''#''.'
      I    = 55

      CALL REPMOT ( LINE, '#', I, 'n',  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCASE)',  OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
