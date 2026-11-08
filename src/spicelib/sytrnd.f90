!$Procedure SYTRND (Transpose two values associated with a symbol)

      subroutine sytrnd ( name, idx1, idx2, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Transpose two values associated with a particular symbol in a
!     double precision symbol table.
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

      integer(ip), parameter :: lbcell = -5

      character(*)          name
      integer(ip)           idx1
      integer(ip)           idx2
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      real(wp)              tabval     ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose associated values are to
!                    be transposed.
!     IDX1       I   Index of first associated value to be transposed.
!     IDX2       I   Index of second associated value to be transposed.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose associated values are
!              to be transposed. If NAME is not in the symbol table,
!              the symbol tables are not modified.
!
!     IDX1     is the index of the first associated value to be
!              transposed. If this index is not valid (IDX1 < 1 or
!              IDX1 > the dimension of NAME) the symbol table is not
!              modified.
!
!     IDX2     is the index of the second associated value to be
!              transposed. If this index is not valid (IDX2 < 1 or
!              IDX2 > the dimension of NAME) the symbol table is not
!              modified.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the double precision symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the double precision symbol table.
!
!              If the symbol NAME is not in the symbol table the symbol
!              tables are not modified. Otherwise, the values that IDX1
!              and IDX2 refer to are transposed in the value table.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If IDX1 < 1, IDX2 < 1, IDX1 > the dimension of NAME, or
!         IDX2 > the dimension of NAME, the error SPICE(INVALIDINDEX)
!         is signaled.
!
!     2)  If NAME is not in the symbol table, the symbol tables are not
!         modified.
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
!        DELTA_T_A     -->    3.2184D1
!        K             -->    1.657D-3
!        MEAN_ANOM     -->    6.239996D0
!                             1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!        BODY4_POLE_RA -->    3.17681D2
!                             1.08D-1
!                             0.0D0
!
!     The call,
!
!     CALL SYTRND ( 'BODY4_POLE_RA', 2, 3, TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        DELTA_T_A     -->    3.2184D1
!        K             -->    1.657D-3
!        MEAN_ANOM     -->    6.239996D0
!                             1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!        BODY4_POLE_RA -->    3.17681D2
!                             0.0D0C
!                             1.08D-1
!     The next call,
!
!     CALL SYTRND ( 'MEAN_ANOM', 2, 4, TABSYM, TABPTR, TABVAL )
!
!     causes the error SPICE(INVALIDINDEX) to be signaled.
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 08-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Changed the name of input arguments "I" and "J" to "IDX1" and
!        "IDX2" for consistency with other routines.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
!
!        Updated so no "exchange" occurs if IDX1 equals IDX2.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
!
!-&

! $ Index_Entries
!
!     transpose two values associated with a symbol
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
!
!        Updated so no "exchange" occurs if IDX1 equals IDX2.
!
!-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
!
!         If one of the indices of the values to be transposed is
!         invalid, an error is signaled and the symbol table is
!         not modified.
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           bsrchc
      integer(ip)           cardc
      logical               return
      integer(ip)           sumai

!
!     Local variables
!
      integer(ip)           nsym

      integer(ip)           locsym
      integer(ip)           locval
      integer(ip)           n

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     How many symbols?
!
      nsym = cardc ( tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

      if ( locsym > 0 ) then

!
!        Are there enough values associated with the symbol?
!
         n = tabptr(locsym)

!
!        Are the indices valid?
!

         if (        idx1 >= 1 &
              .and.  idx1 <= n &
              .and.  idx2 >= 1 &
              .and.  idx2 <= n ) then

!
!           Exchange the values in place.
!
            if ( idx1 /= idx2 ) then

               locval = sumai ( tabptr(1), locsym-1 ) + 1

               call swapd ( tabval(locval+idx1-1), &
                            tabval(locval+idx2-1) )

            end if

         else

            call setmsg ( 'The first index was *. ' &
            //            'The second index was *.' )
            call errint ( '*', idx1                 )
            call errint ( '*', idx2                 )
            call sigerr ( 'SPICE(INVALIDINDEX)'     )

         end if

      end if

      return
      end
