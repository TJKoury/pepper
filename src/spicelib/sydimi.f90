!$Procedure SYDIMI ( Return the dimension of a symbol )

      integer(ip) function sydimi ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the dimension of a particular symbol in an integer symbol
!     table. If the symbol is not found, the function returns the
!     value zero.
!
! $ Required_Reading
!
!     SYMBOLS
!
! $ Keywords
!
!     SYMBOLS
!
! $ Declarations

      implicit none

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      character(*)          name
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      integer(ip)           tabval     ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose dimension is desired.
!     TABSYM,
!     TABPTR,
!     TABVAL     I   Components of the symbol table.
!
!     The function returns the dimension of the symbol NAME. If NAME is
!     not in the symbol table, the function returns the value zero.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose dimension is to be
!              returned. If the symbol is not in the symbol table, the
!              function returns the value zero. This function is case
!              sensitive, NAME must match a symbol exactly.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of an integer symbol table. The table
!              may or may not contain the symbol NAME.
!
! $ Detailed_Output
!
!     The function returns the dimension of the symbol NAME. The
!     dimension of a symbol is the number of values associated with that
!     symbol. If NAME is not in the symbol table, the function returns
!     the value zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        books   -->   5
!                      8
!        erasers -->   6
!        pencils -->  12
!        pens    -->  10
!                     12
!                     24
!
!     Let NUMVAL be equal to the dimension of the symbols in the table.
!     The following code returns the values of NUMVAL indicated in the
!     table.
!
!     NUMVAL = SYDIMI ( 'books',    TABSYM, TABPTR, TABVAL )
!     NUMVAL = SYDIMI ( 'pencils',  TABSYM, TABPTR, TABVAL )
!     NUMVAL = SYDIMI ( 'pens',     TABSYM, TABPTR, TABVAL )
!     NUMVAL = SYDIMI ( 'erasers',  TABSYM, TABPTR, TABVAL )
!     NUMVAL = SYDIMI ( 'tablets',  TABSYM, TABPTR, TABVAL )
!
!
!     ----SYMBOL----------NUMVAL------
!     | books        |       2       |
!     | pencils      |       1       |
!     | pens         |       3       |
!     | erasers      |       1       |
!     | tablets      |       0       |
!     --------------------------------
!
!     Note that the dimension of "tablets" is zero. This is due to the
!     fact that "tablets" is not in the symbol table.
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
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 07-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Updated
!        $Brief_I/O to indicate that TABSYM, TABPTR, TABVAL are input
!        arguments.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        If the value of the function RETURN is .TRUE. upon execution of
!        this module, this function is assigned a default value of
!        either 0, 0.0D0, .FALSE., or blank depending on the type of
!        the function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     fetch the dimension of a symbol
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           bsrchc
      integer(ip)           cardc
      logical               return

!
!     Local variables
!
      integer(ip)           nsym
      integer(ip)           locsym



!
!     Standard SPICE error handling
!
      if ( return() ) then
         sydimi = 0
         return
      else
         call chkin ( 'SYDIMI' )
      end if


!
!     How many symbols to start with?
!
      nsym = cardc ( tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, return zero. Otherwise, look up
!     the dimension directly.
!
      if ( locsym == 0 ) then
         sydimi = 0

      else
         sydimi = tabptr(locsym)
      end if


      call chkout ( 'SYDIMI' )
      return
      end
