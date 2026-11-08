!$Procedure SYRENI ( Rename an existing symbol )

      subroutine syreni ( old, new, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Rename an existing symbol in an integer symbol table.
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

      character(*)          old
      character(*)          new
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      integer(ip)           tabval     ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OLD        I   Name of the symbol to be renamed.
!     NEW        I   New name of the symbol.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     OLD      is the name of the symbol to be renamed. If OLD is
!              not in the symbol table, the tables are not modified.
!
!     NEW      is the new name of the symbol. If the symbol NEW
!              already exists in the symbol table, it is deleted.
!              OLD is then renamed to NEW.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the integer symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the integer symbol table.
!              The values previously associated with OLD are now
!              associated with NEW. If OLD is not in the symbol
!              table, the symbol tables are not modified.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the symbol OLD is not in the symbol table, the error
!         SPICE(NOSUCHSYMBOL) is signaled.
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
!                     10
!        erasers -->   6
!        pencils -->  12
!                     18
!        pens    -->  10
!                     12
!                     24
!
!     The call,
!
!     CALL SYRENI ( 'pens', 'desks', TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        books   -->   5
!                     10
!        desks   -->  10
!                     12
!                     24
!        erasers -->   6
!        pencils -->  12
!                     18
!
!
!     The next call,
!
!     CALL SYRENI ( 'erasers', 'desks', TABSYM, TABPTR, TABVAL )
!
!     modifies the contents fo the symbol table to be:
!
!        books   -->   5
!                     10
!        desks   -->   6
!        pencils -->  12
!                     18
!
!     Note that the symbol "desks" was deleted from the table,
!     and the symbol "erasers" was then renamed to "STRASSMAN". If the
!     new symbol exists, it is deleted from the table before its name
!     is given to another symbol.
!
!
!     The next call,
!
!     CALL SYRENI ( 'chairs', 'stools', TABSYM, TABPTR, TABVAL )
!
!     does not modify the contents of the symbol table. However, it
!     does signal the error SPICE(NOSUCHSYMBOL) because the symbol
!     "chairs" does not exist in the symbol table.
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
!-    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICT NONE statement.
!
!        Edited the header to comply with NAIF standard. Fixed I/O type
!        of arguments TABSYM, TABPTR and TABVAL in $Brief_I/O table.
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
!     rename an existing symbol
!
!-&


! $ Revisions
!
!-     Beta Version 2.0.0, 28-DEC-1989 (HAN)
!
!         Changed the call to SYDELD to a call to SYDELI. The variable
!         TABVAL of type character was being passed to a dummy argument
!         of type double precision.
!
!-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
!
!         Declaration of the unused function SIZEC removed.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           bsrchc
      integer(ip)           cardc
      integer(ip)           lstlec
      logical               return
      integer(ip)           sumai

!
!     Local variables
!
      integer(ip)           nsym

      integer(ip)           oldloc
      integer(ip)           oldval
      integer(ip)           olddim
      integer(ip)           newloc
      integer(ip)           newval



!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Where was the old symbol?
!
      nsym   = cardc  ( tabsym )
      oldloc = bsrchc ( old, nsym, tabsym(1) )

!
!     An overflow is simply not possible here. The only thing that can
!     go wrong is that the old symbol does not exist.
!
      if ( oldloc == 0 ) then

         call setmsg ( 'SYRENI: The symbol # is not in the symbol ' // &
                       'table.' )
         call errch  ( '#', old )
         call sigerr ( 'SPICE(NOSUCHSYMBOL)' )

!
!     Are these the same symbol?
!
      else if ( new /= old ) then

!
!        If the new symbol already exists, delete it.
!
         call sydeli ( new, tabsym, tabptr, tabval )

         nsym   = cardc  (            tabsym    )
         oldloc = bsrchc ( old, nsym, tabsym(1) )

!
!        Swap N elements at the old location with zero elements
!        at the new location.
!
         newloc = lstlec ( new, nsym, tabsym(1) ) + 1

         oldval = sumai ( tabptr(1), oldloc-1 ) + 1
         newval = sumai ( tabptr(1), newloc-1 ) + 1

         olddim = tabptr(oldloc)

         call swapai ( olddim, oldval, 0, newval, tabval(1) )

!
!        Move the name and dimension the same way.
!
         call swapac ( 1, oldloc, 0, newloc, tabsym(1) )
         call swapai ( 1, oldloc, 0, newloc, tabptr(1) )

         if ( oldloc < newloc ) then
            newloc = newloc - 1
         end if

         tabsym(newloc) = new

      end if


      return
      end
