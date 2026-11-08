C$Procedure F_REPMX ( Family of tests for replace marker routines )

      SUBROUTINE F_REPMX ( OK )

C$ Abstract
C
C     Perform a series of tests on the REPMC, REPMCT, REPMD,
C     REPMF, REPMI, REPML and REPMOT routines.
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

      LOGICAL  OK

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
C     This routine tests a family of related routines for inserting
C     values into strings:
C
C        REPMC
C        REPMCT
C        REPMD
C        REPMF
C        REPMI
C        REPML
C        REPMOT
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     M. Liukis          (JPL)
C     B.V. Semenov       (JPL)
C     I.M. Underwood     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 20-JAN-2021 (NJB) (JDR) (BVS) (ML)
C
C        Updated header with missing sections. Added test cases
C        for the REPMCT, REPML, REPMOT routines.
C
C-    TSPICE Version 1.0.0, 30-AUG-1990 (NJB) (IMU)
C
C-&


C$ Index_Entries
C
C     Test family for the replace marker routines
C
C-&


C
C     Local Variables
C
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

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_REPMX' )

C
C     *****************************************************************
C
C     Normal cases: REPMC
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Character substitution  -- check REPMC' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''ok''.'
      SUB    = 'ok'

      CALL REPMC ( LINE, '#', SUB, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Character substitution in-place -- check REPMC' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''ok''.'
      SUB    = 'ok'

      CALL REPMC ( RESULT, '#', SUB, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Character substitution truncated -- check REPMC' )

      LINE    = 'The value is ''#''.'
      EXPLIN  = 'The value is ''a very long string indeed''.'
      SUB     = 'a very long string indeed'

      CALL REPMC ( LINE, '#', SUB, WORD )

      WEXP = EXPLIN(:WDSIZE)

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Character substitution in-place truncated '
     .//           '-- check REPMC' )

      WORD   = 'The value is ''#''.'
      EXPLIN = 'The value is ''a very long string indeed''.'
      SUB    = 'a very long string indeed'

      CALL REPMC ( WORD, '#', SUB, WORD )

      WEXP = EXPLIN(:WDSIZE)

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )

C
C     *****************************************************************
C
C     Normal cases: REPMI
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Integer substitution -- check REPMI' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''3''.'
      I      = 3

      CALL REPMI ( LINE, '#', I, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Integer substitution in-place -- check REPMI' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''3''.'
      I      = 3

      CALL REPMI ( RESULT, '#', I, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     *****************************************************************
C
C     Normal cases: REPMD
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('Double precision substitution (scientific) -- REPMD')

      LINE   = 'The value is ''#''.'
      EXPLIN ='The value is ''4.0E+00''.'
      X      = 4.D0

      CALL REPMD ( LINE, '#', X, 2, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Double precision substitution in-place '
     .//           '(scientific) -- REPMD' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''4.0E+00''.'
      X      = 4.D0

      CALL REPMD ( LINE, '#', X, 2, RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     *****************************************************************
C
C     Normal cases: REPMF
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('Double precision substitution (float) -- REPMF')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''4.0''.'
      X      = 4.D0

      CALL REPMF ( LINE, '#', X, 2, 'f',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Double precision substitution in-place '
     .//           '(float) -- REPMF' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''4.0''.'
      X      = 4.D0

      CALL REPMF ( RESULT, '#', X, 2, 'g',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     *****************************************************************
C
C     Normal cases: REPML
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('Logical value substitution (empty marker) '
     .//          '-- check REPML')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'

      CALL REPML ( LINE, ' ', .TRUE., 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Logical value substitution '
     .//           '(non-existent marker) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'

      CALL REPML ( LINE, 'abc', .TRUE., 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Logical value substitution '
     .//           '(first marker, TRUE) -- check REPML' )

      LINE   = 'The values are ''#'' and ''#''.'
      EXPLIN = 'The values are ''TRUE'' and ''#''.'

      CALL REPML ( LINE, '#', .TRUE., 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Logical value substitution truncated '
     .//           '-- check REPML' )

      LINE   = 'The values are long ''#'' and ''#''.'
      EXPLIN = 'The values are long ''TRUE'' and ''#''.'
      WEXP   = EXPLIN( 1:WDSIZE )

      CALL REPML ( LINE, '#', .TRUE., 'u',  WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Logical value substitution in-place '
     .//           '(TRUE) -- check REPML' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''TRUE''.'

      CALL REPML ( RESULT, '#', .TRUE., 'U',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Logical value substitution in-place '
     .//           ' truncated -- check REPML' )

      WORD   = 'The value is ''#'' and ''#''.'
      EXPLIN = 'The value is ''TRUE'' and ''#''.'
      WEXP   = EXPLIN( 1:WDSIZE )

      CALL REPML ( WORD, '#', .TRUE., 'U',  WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Logical value substitution '
     .//           '(True) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''True''.'

      CALL REPML ( LINE, '#', .TRUE., 'c',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Logical value substitution '
     .//           '(true) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''true''.'

      CALL REPML ( LINE, '#', .TRUE., 'l',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Logical value substitution '
     .//           '(FALSE) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''FALSE''.'

      CALL REPML ( LINE, '#', .FALSE., 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Logical value substitution '
     .//           '(False) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''False''.'

      CALL REPML ( LINE, '#', .FALSE., 'c',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Logical value substitution '
     .//           '(false) -- check REPML' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''false''.'

      CALL REPML ( LINE, '#', .FALSE., 'l',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     *****************************************************************
C
C     Normal cases: REPMCT
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('Cardinal text substitution (empty marker) '
     .//          '-- check REPMCT')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'
      I      = 55

      CALL REPMCT ( LINE, ' ', I, 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('Cardinal text substitution '
     .//          '(non-existent marker) -- check REPMCT')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'
      I      = 55

      CALL REPMCT ( LINE, 'abc', I, 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Cardinal text substitution, lowercase '
     .//           '-- check REPMCT' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''fifty-five''.'
      I      = 55

      CALL REPMCT ( LINE, '#', I, 'l', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Cardinal text substitution, uppercase '
     .//           '-- check REPMCT' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''FIFTY-FIVE''.'
      I      = 55

      CALL REPMCT ( LINE, '#', I, 'U', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Cardinal text substitution, capitalized '
     .//           '-- check REPMCT' )

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''Fifty-five''.'
      I      = 55

      CALL REPMCT ( LINE, '#', I, 'c', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Cardinal text substitution truncated '
     .//           '-- check REPMCT' )

      LINE    = 'The value is ''#''.'
      EXPLIN  = 'The value is ''seven hundred seven''.'
      WEXP    = EXPLIN( 1:WDSIZE )

      I       = 777 777 777

      CALL REPMCT ( LINE, '#', I, 'l', WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Cardinal text substitution in-place '
     .//           '-- check REPMCT' )

      RESULT = 'The value is ''#''.'
      EXPLIN = 'The value is ''fifty-five''.'
      I      = 55

      CALL REPMCT ( RESULT, '#', I, 'L', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Cardinal text substitution in-place truncated '
     .//           '-- check REPMCT' )

      WORD   = 'The value is ''#''.'
      EXPLIN = 'The value is ''seven hundred seven''.'
      WEXP   = EXPLIN( 1:WDSIZE )

      I      = 777 777 777

      CALL REPMCT ( WORD, '#', I, 'l', WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


C
C     *****************************************************************
C
C     Normal cases: REPMOT
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('Ordinal text substitution (empty marker) '
     .//          '-- check REPMOT')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'
      I      = 55

      CALL REPMOT ( LINE, ' ', I, 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('Ordinal text substitution '
     .//          '(non-existent marker) -- check REPMOT')

      LINE   = 'The value is ''#''.'
      EXPLIN = 'The value is ''#''.'
      I      = 55

      CALL REPMOT ( LINE, 'abc', I, 'u',  RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Ordinal text substitution, lowercase '
     .//           '-- check REPMOT' )

      LINE   = 'The ''#'' value is 55.'
      EXPLIN = 'The ''fifty-fifth'' value is 55.'
      I      = 55

      CALL REPMOT ( LINE, '#', I, 'l', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Ordinal text substitution, uppercase '
     .//           '-- check REPMOT' )

      LINE   = 'The ''#'' value is 55.'
      EXPLIN = 'The ''FIFTY-FIFTH'' value is 55.'
      I      = 55

      CALL REPMOT ( LINE, '#', I, 'U', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Ordinal text substitution, capitalized '
     .//           '-- check REPMOT' )

      LINE   = 'The ''#'' value is 55.'
      EXPLIN = 'The ''Fifty-fifth'' value is 55.'
      I      = 55

      CALL REPMOT ( LINE, '#', I, 'c', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Ordinal text substitution truncated '
     .//           '-- check REPMOT' )

      LINE   = 'The ''#'' value is 777 777 777.'
      EXPLIN = 'The ''seven hundred seventy-seven million''.'
      WEXP   = EXPLIN( 1:WDSIZE )

      I      = 777 777 777

      CALL REPMOT ( LINE, '#', I, 'l', WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Ordinal text substitution in-place '
     .//           '-- check REPMOT' )

      RESULT = 'The ''#'' value is 77.'
      EXPLIN = 'The ''seventy-seventh'' value is 77.'
      I      = 77

      CALL REPMOT ( RESULT, '#', I, 'L', RESULT )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'RESULT', RESULT, '=', EXPLIN, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Ordinal text substitution in-place truncated '
     .//           '-- check REPMOT' )

      WORD   = 'The ''#'' value is 77.'
      EXPLIN = 'The ''seventy-seventh'' value is 77.'
      WEXP   = EXPLIN( 1:WDSIZE )

      I      = 77

      CALL REPMOT ( WORD, '#', I, 'l', WORD )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSC ( 'WORD', WORD, '=', WEXP, OK )


C
C     *****************************************************************
C
C     Error cases: REPMCT
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Cardinal text substitution '
     .//           '(invalid case flag) -- check REPMCT' )

      LINE = 'The value is ''#''.'
      I    = 55

      CALL REPMCT ( LINE, '#', I, 'n',  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCASE)',  OK )


C
C     *****************************************************************
C
C     Error cases: REPML
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Logical value substitution '
     .//           '(invalid case flag) -- check REPML' )

      LINE = 'The value is ''#''.'

      CALL REPML  ( LINE, '#', .TRUE., 'n',  RESULT   )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCASE)',  OK )

C
C     *****************************************************************
C
C     Error cases: REPMOT
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Ordinal text substitution '
     .//           '(invalid case flag) -- check REPMOT' )

      LINE = 'The value is ''#''.'
      I    = 55

      CALL REPMOT ( LINE, '#', I, 'n',  RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCASE)',  OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
