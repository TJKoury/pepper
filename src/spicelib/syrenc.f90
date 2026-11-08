!$Procedure SYRENC ( Rename an existing symbol )

      subroutine syrenc ( old, new, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Rename an existing symbol in a character symbol table.
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

      character(*)          old
      character(*)          new
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      character(*)          tabval     ( lbcell:* )

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
!     TABVAL   are components of the character symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the character symbol table.
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
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!        FERMI     -->   NUCLEAR FISSION
!        HAHN      -->   NUCLEAR FISSION
!        PAULI     -->   EXCLUSION PRINCIPLE
!                        NEUTRINO
!
!     The call,
!
!     CALL SYRENC ( 'FERMI', 'STRASSMAN', TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!        HAHN      -->   NUCLEAR FISSION
!        PAULI     -->   EXCLUSION PRINCIPLE
!                        NEUTRINO
!        STRASSMAN -->   NUCLEAR FISSION
!
!
!     The next call,
!
!     CALL SYRENC ( 'HAHN', 'STRASSMAN', TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!        PAULI     -->   EXCLUSION PRINCIPLE
!                        NEUTRINO
!        HAHN      -->   NUCLEAR FISSION
!
!     Note that the symbol "STRASSMAN" was deleted from the table,
!     and the symbol "HAHN" was then renamed to "STRASSMAN". If the
!     new symbol exists, it is deleted from the table before its name
!     is given to another symbol.
!
!
!     The next call,
!
!     CALL SYRENC ( 'FERMI', 'HAHN', TABSYM, TABPTR, TABVAL )
!
!     does not modify the contents of the symbol table. It signals
!     the error SPICE(NOSUCHSYMBOL) because the symbol "FERMI" does
!     not exist in the symbol table.
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
!-    Beta Version 2.0.0, 28-DEC-1989 (HAN)
!
!        Changed the call to SYDELD to a call to SYDELC. The variable
!        TABVAL of type character was being passed to a dummy argument
!        of type double precision.
!
!-    Beta Version 1.1.0, 17-FEB-1989 (NJB)
!
!        Declaration of the unused function SIZEC removed.
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

         call setmsg ( 'SYRENC: The symbol # is not in the symbol ' // &
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
         call sydelc ( new, tabsym, tabptr, tabval )

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

         call swapac ( olddim, oldval, 0, newval, tabval(1) )

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
