!$Procedure SYDIMD ( Return the dimension of a symbol )

      integer(ip) function sydimd ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the dimension of a particular symbol in a double precision
!     symbol table. If the symbol is not found, the function returns the
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

      character*(*)         name
      character*(*)         tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      real(wp)              tabval     ( lbcell:* )

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
!     TABVAL   are the components of a double precision symbol table.
!              The table may or may not contain the symbol NAME.
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
!        DELTA_T_A -->   32.184
!        K         -->    1.657D-3
!        MEAN_ANOM -->    6.239996D0
!                         1.99096871D-7
!
!
!     Let NUMVAL be equal to the dimension of the symbols in the table.
!     The following code returns the values of NUMVAL indicated in the
!     table.
!
!     NUMVAL = SYDIMD ( 'MEAN_ANOM',   TABSYM, TABPTR, TABVAL )
!     NUMVAL = SYDIMD ( 'K',           TABSYM, TABPTR, TABVAL )
!     NUMVAL = SYDIMD ( 'DELTA_T_A',   TABSYM, TABPTR, TABVAL )
!     NUMVAL = SYDIMD ( 'BODY10_AXES', TABSYM, TABPTR, TABVAL )
!
!     ----SYMBOL----------NUMVAL------
!     | MEAN_ANOM    |       2       |
!     | K            |       1       |
!     | DELTA_T_A    |       1       |
!     | BODY10_AXES  |       0       |
!     --------------------------------
!
!     Note that the dimension of "BODY10_AXES" is zero. This is due to
!     the fact that "BODY10_AXES" is not in the symbol table.
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
!     Standard SPICE error handling.
!
      if ( return () ) then
         sydimd = 0
         return
      else
         call chkin ( 'SYDIMD' )
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
      if ( locsym .eq. 0 ) then
         sydimd = 0

      else
         sydimd = tabptr(locsym)
      end if


      call chkout ( 'SYDIMD' )
      return
      end
