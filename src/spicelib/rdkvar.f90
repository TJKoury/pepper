!$Procedure RDKVAR ( Read the next variable from a kernel file )

      SUBROUTINE RDKVAR ( TABSYM, TABPTR, TABVAL, NAME, EOF )

! $ Abstract
!
!     Read the next variable from a SPICE ASCII kernel file into a
!     double precision symbol table.
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

      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )
      CHARACTER*(*)         NAME
      LOGICAL               EOF

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Symbol table.
!     NAME       O   Name of the variable.
!     EOF        O   End of file indicator.
!     LINLEN     P   Maximum line length.
!
! $ Detailed_Input
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a double precision symbol table. On
!              input, the table may or may not contain any variables.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   on output, contains the name and values of the next
!              variable in kernel file. Depending on the assignment
!              directive, the values in the file may replace or augment
!              any existing values.
!
!      NAME    is the name of the variable. NAME is blank if no variable
!              is read.
!
!      EOF     is .TRUE. when the end of the kernel file has been
!              reached, and is .FALSE. otherwise. The kernel file is
!              closed automatically when the end of the file is reached.
!
! $ Parameters
!
!     LINLEN   is the maximum length of a line in the kernel file.
!
! $ Exceptions
!
!     1)  If an error occurs parsing a date from the kernel file, the
!         error SPICE(DATEEXPECTED) is signaled.
!
!     2)  If an error occurs parsing a numeric value from the kernel
!         file, the error SPICE(NUMBEREXPECTED) is signaled.
!
! $ Files
!
!     RDKVAR reads from the file most recently opened by RDKNEW.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     In the following example, RDKNEW and RDKVAR are used to read
!     the contents of two kernel files into a single symbol table.
!     First, the table is cleared.
!
!         CALL SCARDC ( 0, TABSYM )
!         CALL SCARDI ( 0, TABPTR )
!         CALL SCARDD ( 0, TABVAL )
!
!     Next, the files are opened and read individually.
!
!         DO I = 1, 2
!            CALL RDKNEW ( KERNEL(I), EOF )
!
!            DO WHILE ( .NOT. EOF )
!               CALL RDKVAR ( TABSYM, TABPTR, TABVAL, NAME, EOF )
!            END DO
!         END DO
!
!     Let the files KERNEL(1) and KERNEL(2) contain
!
!         ===========================================================
!
!         \begindata
!         DELTA_T_A       =   32.184
!         K               =    1.657D-3
!         ORBIT_ECC       =    1.671D-2
!         MEAN_ANOM       = (  6.239996D0,  1.99096871D-7 )
!
!         ===========================================================
!
!     and
!
!         ===========================================================
!         \begindata
!          K               =    0.0D0
!         ===========================================================
!
!     respectively. Then the contents of the symbol table are
!
!          DELTA_T_A  -->   32.184
!          K          -->    0.0D0
!          MEAN_ANOM  -->    6.239996D0
!                            1.99096871D-7
!          ORBIT_ECC  -->    1.671D-2
!
!     In particular, the value of K read from the second file replaces
!     the value read from the first file.
!
! $ Restrictions
!
!     1)  The input file must be opened and initialized by RDKNEW prior
!         to the first call to RDKVAR.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 10-MAR-1992 (WLT)
!
!        Changed the length of the local character variable ERROR so
!        that it would always have a length greater than the lengths of
!        the character string values placed into it.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     read the next variable from a kernel file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 10-MAR-1992 (WLT)
!
!        Changed the length of the local character variable ERROR so
!        that it would always have a length greater than the lengths of
!        the character string values placed into it.
!
!        The length of the character variable ERROR was changed from 30
!        to 80.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!-    Beta Version 2.0.0, 23-OCT-1989 (HAN)
!
!        Added a test to FAILED in the main DO-loop to prevent
!        infinite looping. If the error mode was set to 'RETURN'
!        and an error occurred, the same line could be processed
!        forever.
!
!-    Beta Version 1.1.0, 13-JAN-1989 (IMU)
!
!        Variable name may now take up an entire line. The previous
!        maximum length (32 characters) was tied to the known length
!        used by POOL. That length is now parameterized. Rather than
!        have two parameters, which could get out of synch, RDKVAR
!        now assumes that a variable name can be as long as an input
!        line.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
      LOGICAL               FAILED

!
!     Local variables
!
      CHARACTER*1           COMMA
      PARAMETER           ( COMMA = ',' )

      CHARACTER*1           BLANK
      PARAMETER           ( BLANK = ' ' )

      CHARACTER*1           LPAREN
      PARAMETER           ( LPAREN = '(' )

      CHARACTER*1           RPAREN
      PARAMETER           ( RPAREN = ')' )

      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    VARNAM
      CHARACTER*3           DIRCTV
      CHARACTER*6           STATUS

      CHARACTER*30          CVALUE
      DOUBLE PRECISION      DVALUE
      INTEGER               I

      CHARACTER*80          ERROR



!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RDKVAR' )
      END IF

!
!     No variable yet.
!
      NAME  = ' '


!
!     No parsing error has occurred yet.
!
      ERROR = ' '


!
!     Get the next data line. Unless something is terribly wrong,
!     this will begin a new variable definition. We have to read
!     the whole variable, unless we luck out and get an error, in
!     which case we can quit.
!
      STATUS = 'BEGIN'

      DO WHILE ( ( STATUS .NE. 'DONE' ) .AND. ( .NOT. FAILED() ) )

         CALL RDKDAT ( LINE, EOF )

         IF ( EOF ) THEN
            CALL CHKOUT ( 'RDKVAR' )
            RETURN
         END IF

!
!        Replace commas with blanks. We make no distinctions between
!        the two.
!
         CALL REPLCH ( LINE, COMMA, BLANK, LINE )

!
!        The first word on the first line should be the name of a
!        variable. The second word should be a directive: = or +=.
!
         IF ( STATUS .EQ. 'BEGIN' ) THEN

            CALL NEXTWD ( LINE, VARNAM, LINE )
            CALL NEXTWD ( LINE, DIRCTV, LINE )

!
!           If this is replacement (=) and not an addition (+=),
!           delete the values currently associated with the variable.
!           They will be replaced later.
!
            IF ( DIRCTV .EQ. '=' ) THEN
               CALL SYDELD ( VARNAM, TABSYM, TABPTR, TABVAL )
            END IF

!
!           If this is a vector, the next thing on the line will be a
!           left parenthesis. Otherwise, assume that this is a scalar.
!           If it's a vector, get the first value. If it's a scalar,
!           plant a bogus right parenthesis, to make the following loop
!           terminate after one iteration.
!
            CALL NEXTWD ( LINE, CVALUE, LINE )

            IF ( CVALUE .EQ. LPAREN ) THEN
               CALL NEXTWD ( LINE, CVALUE, LINE )
            ELSE
               LINE = RPAREN
            END IF

!
!        For subsequent lines, treat everything as a new value.
!
         ELSE

            CALL NEXTWD ( LINE, CVALUE, LINE )

         END IF

!
!        We have a value anyway. Store it in the table.
!
!        Keep going until the other shoe (the right parenthesis)
!        drops, or until the end of the line is reached.
!
!        Dates begin with @; anything else is presumed to be a number.
!
         DO WHILE ( CVALUE .NE. RPAREN  .AND.  CVALUE .NE. BLANK )

            IF ( CVALUE(1:1) .EQ. '@' ) THEN

               CALL TPARSE ( CVALUE(2: ), DVALUE, ERROR )

               IF ( ERROR .NE. ' ' ) THEN
                  ERROR  =  'Encountered : ' // CVALUE(2: )
                  CALL SETMSG ( ERROR    )
                  CALL SIGERR ( 'SPICE(DATEEXPECTED)' )
                  CALL CHKOUT ( 'RDKVAR' )
                  RETURN
               END IF

            ELSE

               CALL NPARSD ( CVALUE, DVALUE, ERROR, I )

               IF ( ERROR .NE. ' ' ) THEN
                  ERROR =  'Encountered : ' // CVALUE
                  CALL SETMSG ( ERROR    )
                  CALL SIGERR ( 'SPICE(NUMBEREXPECTED)' )
                  CALL CHKOUT ( 'RDKVAR' )
                  RETURN
               END IF

            END IF

            CALL SYENQD ( VARNAM, DVALUE, TABSYM, &
                                          TABPTR, &
                                          TABVAL )

            CALL NEXTWD ( LINE, CVALUE, LINE )

         END DO

         IF ( CVALUE .EQ. RPAREN ) THEN
            STATUS = 'DONE'
         ELSE
            STATUS = 'INVAR'
         END IF

      END DO

!
!     Return the name of the variable, but only if everything went okay.
!
      NAME = VARNAM


      CALL CHKOUT ( 'RDKVAR' )
      RETURN
      END
