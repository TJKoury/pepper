!$Procedure PRSDP   ( Parse d.p. number with error checking )

      subroutine prsdp ( string, dpval )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Parse a string as a double precision number, encapsulating error
!     handling.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     NUMBER
!     PARSING
!
! $ Declarations

      implicit none

      character(*)          string
      real(wp)              dpval

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   String representing a numeric value.
!     DPVAL      O   D.p. value obtained by parsing STRING.
!
! $ Detailed_Input
!
!     STRING   is a string representing a numeric value. Commas and
!              spaces may be used in this string for ease of reading
!              and writing the number. They are treated as
!              insignificant but non-error-producing characters.
!
!              For exponential representation any of the characters
!              'E','D','e','d' may be used.
!
!              The following are legitimate numeric expressions
!
!                 +12.2 e-1
!                 -3. 1415 9276
!                 1e12
!                 E10
!
!              The program also recognizes the following  mnemonics
!
!                 'PI',  'pi',  'Pi',  'pI'
!                 '+PI', '+pi', '+Pi', '+pI'
!                 '-PI', '-pi', '-Pi', '-pI'
!
!              and returns the value
!
!                 ( + OR - ) 3.1415 9265 3589 7932 3846 26 ...
!
!              as appropriate.
!
! $ Detailed_Output
!
!     DPVAL    is the double precision number obtained by parsing
!              STRING.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input string cannot be parsed  due to use of an
!         unexpected or misplaced character or due to a string
!         representing a number too large for double precision, the
!         error SPICE(NOTADPNUMBER) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The purpose of this routine is to enable safe parsing of double
!     precision numbers without the necessity of in-line error checking.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Parse into a DOUBLE PRECISION variable a set of strings
!        representing numeric values.
!
!
!        Example code begins here.
!
!
!              PROGRAM PRSDP_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               SETSIZ
!              PARAMETER           ( SETSIZ = 8  )
!
!              INTEGER               STRLEN
!              PARAMETER           ( STRLEN = 11 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(STRLEN)    STRVAL ( SETSIZ )
!
!              DOUBLE PRECISION      DPVAL
!
!              INTEGER               I
!
!        C
!        C     Initialize the array of strings.
!        C
!              DATA                  STRVAL / '100,000,000',
!             .                               ' -2 690 192',
!             .                               '  +12.2 e-1',
!             .                               '-3. 141 592',
!             .                               '     1.2e12',
!             .                               '        E10',
!             .                               '         Pi',
!             .                               '        -PI' /
!
!        C
!        C     Parse each string into a DOUBLE PRECISION variable.
!        C
!              WRITE(*,'(A)') '   STRVAL               DPVAL'
!              WRITE(*,'(A)') '-----------  --------------------------'
!              DO I = 1, SETSIZ
!
!                 CALL PRSDP ( STRVAL(I), DPVAL )
!
!                 WRITE(*,'(A11,F28.12)') STRVAL(I), DPVAL
!
!              END DO
!
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!           STRVAL               DPVAL
!        -----------  --------------------------
!        100,000,000      100000000.000000000000
!         -2 690 192       -2690192.000000000000
!          +12.2 e-1              1.220000000000
!        -3. 141 592             -3.141592000000
!             1.2e12  1200000000000.000000000000
!                E10    10000000000.000000000000
!                 Pi              3.141592653590
!                -PI             -3.141592653590
!
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
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 28-MAY-2020 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Updated the header to properly describe its input, output,
!        exceptions and particulars.
!
!-    SPICELIB Version 1.1.0, 15-SEP-1997 (NJB)
!
!        Bug fix: output argument declaration changed from INTEGER
!        to DOUBLE PRECISION.
!
!-    SPICELIB Version 1.0.0, 22-JUL-1997 (NJB)
!
!-&


! $ Index_Entries
!
!     parse d.p. number with encapsulated error handling
!
!-&


!
!     Local parameters
!
      integer(ip)           msglen
      parameter           ( msglen = 320 )

!
!     Local variables
!
      character(msglen)     errmsg
      integer(ip)           ptr


!
!     Use discovery check-in.
!
      call nparsd ( string, dpval, errmsg, ptr )

      if ( errmsg /= ' ' ) then

         call chkin  ( 'PRSDP'               )
         call setmsg ( errmsg                )
         call sigerr ( 'SPICE(NOTADPNUMBER)' )
         call chkout ( 'PRSDP'               )
         return

      end if

      return
      end
