!$Procedure RDKVAR ( Read the next variable from a kernel file )

      subroutine rdkvar ( tabsym, tabptr, tabval, name, eof )
      use pepper_kinds, only: ip, wp

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

      implicit none

      integer(ip), parameter :: lbcell = -5

      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      real(wp)              tabval     ( lbcell:* )
      character(*)          name
      logical               eof

      integer(ip), parameter :: linlen = 80

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
      logical               return
      logical               failed

!
!     Local variables
!
      character(1), parameter :: comma = ','

      character(1), parameter :: blank = ' '

      character(1), parameter :: lparen = '('

      character(1), parameter :: rparen = ')'

      character(linlen)     line
      character(linlen)     varnam
      character(3)          dirctv
      character(6)          status

      character(30)         cvalue
      real(wp)              dvalue
      integer(ip)           i

      character(80)         error



!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     No variable yet.
!
      name  = ' '


!
!     No parsing error has occurred yet.
!
      error = ' '


!
!     Get the next data line. Unless something is terribly wrong,
!     this will begin a new variable definition. We have to read
!     the whole variable, unless we luck out and get an error, in
!     which case we can quit.
!
      status = 'BEGIN'

      do while ( ( status /= 'DONE' ) .and. ( .not. failed() ) )

         call rdkdat ( line, eof )

         if ( eof ) return

!
!        Replace commas with blanks. We make no distinctions between
!        the two.
!
         call replch ( line, comma, blank, line )

!
!        The first word on the first line should be the name of a
!        variable. The second word should be a directive: = or +=.
!
         if ( status == 'BEGIN' ) then

            call nextwd ( line, varnam, line )
            call nextwd ( line, dirctv, line )

!
!           If this is replacement (=) and not an addition (+=),
!           delete the values currently associated with the variable.
!           They will be replaced later.
!
            if ( dirctv == '=' ) then
               call sydeld ( varnam, tabsym, tabptr, tabval )
            end if

!
!           If this is a vector, the next thing on the line will be a
!           left parenthesis. Otherwise, assume that this is a scalar.
!           If it's a vector, get the first value. If it's a scalar,
!           plant a bogus right parenthesis, to make the following loop
!           terminate after one iteration.
!
            call nextwd ( line, cvalue, line )

            if ( cvalue == lparen ) then
               call nextwd ( line, cvalue, line )
            else
               line = rparen
            end if

!
!        For subsequent lines, treat everything as a new value.
!
         else

            call nextwd ( line, cvalue, line )

         end if

!
!        We have a value anyway. Store it in the table.
!
!        Keep going until the other shoe (the right parenthesis)
!        drops, or until the end of the line is reached.
!
!        Dates begin with @; anything else is presumed to be a number.
!
         do while ( cvalue /= rparen  .and.  cvalue /= blank )

            if ( cvalue(1:1) == '@' ) then

               call tparse ( cvalue(2: ), dvalue, error )

               if ( error /= ' ' ) then
                  error  =  'Encountered : ' // cvalue(2: )
                  call setmsg ( error    )
                  call sigerr ( 'SPICE(DATEEXPECTED)' )
                  return
               end if

            else

               call nparsd ( cvalue, dvalue, error, i )

               if ( error /= ' ' ) then
                  error =  'Encountered : ' // cvalue
                  call setmsg ( error    )
                  call sigerr ( 'SPICE(NUMBEREXPECTED)' )
                  return
               end if

            end if

            call syenqd ( varnam, dvalue, tabsym, &
                                          tabptr, &
                                          tabval )

            call nextwd ( line, cvalue, line )

         end do

         if ( cvalue == rparen ) then
            status = 'DONE'
         else
            status = 'INVAR'
         end if

      end do

!
!     Return the name of the variable, but only if everything went okay.
!
      name = varnam


      return
      end
