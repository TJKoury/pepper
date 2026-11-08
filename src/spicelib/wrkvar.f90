!$Procedure WRKVAR ( Write a variable to a kernel file )

      subroutine wrkvar ( unit, name, dirctv, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp

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

      implicit none

      integer(ip), parameter :: lbcell = -5

      integer(ip)           unit
      character(*)          name
      character(*)          dirctv
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      real(wp)              tabval     ( lbcell:* )

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
      integer(ip)           sydimd
      logical               return

!
!     Local variables
!
      character(1), parameter :: blank = ' '

      character(1), parameter :: lparen = '('

      character(3), parameter :: rparen = '  )'

      character(2), parameter :: comma = ','

      integer(ip)           varlen
      integer(ip)           margin
      integer(ip)           vardim

      character(132)        line
      real(wp)              dvalue

      logical               found
      integer(ip)           iostat
      integer(ip)           i

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Preliminary measurements.
!
      varlen = len ( tabsym(1) )
      margin = varlen + 6
      vardim = sydimd ( name, tabsym, tabptr, tabval )

!
!     One value per line.
!
      do i = 1, vardim

         call synthd ( name, i, tabsym, tabptr, tabval, dvalue, found )

!
!        The first line contains the variable name, the directive,
!        an optional left parenthesis, and the first value. The values
!        of a multi-dimensional variable are separated by commas.
!
         if ( i == 1 ) then

            call ljust ( name,   line                    )
            call rjust ( dirctv, line(margin-4:margin-3) )

            if ( vardim > 1 ) then

               line(margin-1:margin-1) = lparen
               write (unit,*,iostat=iostat) line(1:margin), dvalue, &
                                            comma

            else

               write (unit,*,iostat=iostat) line(1:margin), dvalue

            end if

!
!        The last line of a multi-dimensional variable ends with a
!        right parenthesis.
!
         else if ( i > 1  .and.  i == vardim ) then

            line = ' '
            write (unit,*,iostat=iostat) line(1:margin), dvalue, rparen

         else

            line = ' '
            write (unit,*,iostat=iostat) line(1:margin), dvalue, comma

         end if

      end do

      if ( iostat /= 0 ) then
         call ioerr ( 'writing a variable to the output kernel file', &
                      ' ', &
                      iostat                                          )

         call sigerr ( 'SPICE(WRITEERROR)' )

      end if

      return
      end
