!$Procedure WRKVAR ( Write a variable to a kernel file )

      SUBROUTINE WRKVAR ( UNIT, NAME, DIRCTV, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Write the value of a variable in a double precision symbol
!     table to a NAIF ASCII kernel file.
!
! $ Required_Reading
!
!     KERNEL
!     SYMBOLS
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               UNIT
      CHARACTER*(*)         NAME
      CHARACTER*(*)         DIRCTV
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Output logical unit.
!     NAME       I   Name of the variable.
!     DIRCTV     I   Kernel directive: '=' or '+='.
!     TABSYM,
!     TABPTR,
!     TABVAL     I   Double precision symbol table.
!
! $ Detailed_Input
!
!     UNIT     is the logical unit to which the variable will be
!              written. This is usually the logical unit to which the
!              output kernel file is connected.
!
!     NAME     is the name of the variable to be written to UNIT.
!
!     DIRCTV   is the directive linking NAME and its associated values
!              in the kernel file. This may be any of the directives
!              recognized by RDKVAR.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a double precision symbol table.
!
!              On input, the table may or may not contain any variables.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs writing the variable to UNIT, the error
!         SPICE(WRITEERROR) is signaled.
!
! $ Files
!
!     If the variable is to be written to an output kernel file, the
!     file should be opened with a logical unit determined by the
!     calling program.
!
! $ Particulars
!
!     If the table symbol table does not contain any variables, nothing
!     will be written to UNIT.
!
! $ Examples
!
!     If  NAME   = 'MEAN_ANOM'
!         DIRCTV = '='
!
!     And the contents of the symbol table are:
!
!          DELTA_T_A  -->   32.184
!          K          -->    0.D0
!          MEAN_ANOM  -->    6.239996D0
!                            1.99096871D-7
!          ORBIT_ECC  -->    1.671D-2
!
!     The output to UNIT might look like this, depending on the
!     length of the symbol table variables:
!
!        MEAN_ANOM   = ( 6.239996D0,
!                        1.99096871D-7  )
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
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Updated $Brief_I/O entry for symbol table (TABSYM, TABPTR,
!        TABVAR) to indicate that these arguments only are an input to
!        the routine, and not an output. Removed $Detailed_Output
!        documentation, as the routine operates by side-effects and has
!        no output arguments.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (NJB)
!
!-&


! $ Index_Entries
!
!     write a variable to a kernel file
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 20-DEC-1988 (NJB)
!
!        Call to IOERR changed to be consistent with new calling
!        protocol. SETMSG call deleted, since IOERR now calls SETMSG.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               SYDIMD
      LOGICAL               RETURN

!
!     Local variables
!
      CHARACTER*1           BLANK
      PARAMETER           ( BLANK = ' '    )

      CHARACTER*1           LPAREN
      PARAMETER           ( LPAREN = '('   )

      CHARACTER*3           RPAREN
      PARAMETER           ( RPAREN = '  )' )

      CHARACTER*2           COMMA
      PARAMETER           ( COMMA  = ','   )

      INTEGER               VARLEN
      INTEGER               MARGIN
      INTEGER               VARDIM

      CHARACTER*132         LINE
      DOUBLE PRECISION      DVALUE

      LOGICAL               FOUND
      INTEGER               IOSTAT
      INTEGER               I


!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WRKVAR' )
      END IF


!
!     Preliminary measurements.
!
      VARLEN = LEN ( TABSYM(1) )
      MARGIN = VARLEN + 6
      VARDIM = SYDIMD ( NAME, TABSYM, TABPTR, TABVAL )

!
!     One value per line.
!
      DO I = 1, VARDIM

         CALL SYNTHD ( NAME, I, TABSYM, TABPTR, TABVAL, DVALUE, FOUND )

!
!        The first line contains the variable name, the directive,
!        an optional left parenthesis, and the first value. The values
!        of a multi-dimensional variable are separated by commas.
!
         IF ( I .EQ. 1 ) THEN

            CALL LJUST ( NAME,   LINE                    )
            CALL RJUST ( DIRCTV, LINE(MARGIN-4:MARGIN-3) )

            IF ( VARDIM .GT. 1 ) THEN

               LINE(MARGIN-1:MARGIN-1) = LPAREN
               WRITE (UNIT,*,IOSTAT=IOSTAT) LINE(1:MARGIN), DVALUE, &
                                            COMMA

            ELSE

               WRITE (UNIT,*,IOSTAT=IOSTAT) LINE(1:MARGIN), DVALUE

            END IF


!
!        The last line of a multi-dimensional variable ends with a
!        right parenthesis.
!
         ELSE IF ( I .GT. 1  .AND.  I .EQ. VARDIM ) THEN

            LINE = ' '
            WRITE (UNIT,*,IOSTAT=IOSTAT) LINE(1:MARGIN), DVALUE, RPAREN

         ELSE

            LINE = ' '
            WRITE (UNIT,*,IOSTAT=IOSTAT) LINE(1:MARGIN), DVALUE, COMMA

         END IF

      END DO

      IF ( IOSTAT .NE. 0 ) THEN
         CALL IOERR ( 'writing a variable to the output kernel file', &
                      ' ', &
                      IOSTAT                                          )

         CALL SIGERR ( 'SPICE(WRITEERROR)' )

      END IF


      CALL CHKOUT ( 'WRKVAR' )
      RETURN
      END
