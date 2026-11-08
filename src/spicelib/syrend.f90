!$Procedure SYREND ( Rename an existing symbol )

      SUBROUTINE SYREND ( OLD, NEW, TABSYM, TABPTR, TABVAL )

! $ Abstract
!
!     Rename an existing symbol in a double precision symbol table.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)         OLD
      CHARACTER*(*)         NEW
      CHARACTER*(*)         TABSYM     ( LBCELL:* )
      INTEGER               TABPTR     ( LBCELL:* )
      DOUBLE PRECISION      TABVAL     ( LBCELL:* )

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
!     TABVAL   are components of the double precision symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are components of the double precision symbol table.
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
!        BODY4_POLE_RA -->    3.17681D2
!                             1.08D-1
!                             0.0D0
!        DELTA_T_A     -->    3.2184D1
!        K             -->    1.657D-3
!        MEAN_ANOM     -->    6.239996D0
!                             1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!
!
!     The call,
!
!     CALL SYREND ( 'K', 'EB', TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        BODY4_POLE_RA -->    3.17681D2
!                             1.08D-1
!                             0.0D0
!        DELTA_T_A     -->    3.2184D1
!        EB            -->    1.657D-3
!        MEAN_ANOM     -->    6.239996D0
!                             1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!                             1.08D-1
!                             0.0D0
!
!     The next call,
!
!     CALL SYREND ( 'EB', 'DELTA_T_A', TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the table to be:
!
!        BODY4_POLE_RA -->    3.17681D2
!                             1.08D-1
!                             0.0D0
!        DELTA_T_A     -->    1.657D-3
!        MEAN_ANOM     -->    6.239996D0
!                             1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!                             1.08D-1
!                             0.0D0
!
!     Note that the symbol "DELTA_T_A" was deleted from the table,
!     and the symbol "EB" was then renamed to "DELTA_T_A". If the
!     new symbol exists, it is deleted from the table before its name
!     is given to another symbol.
!
!
!     The next call,
!
!     CALL SYREND ( 'K', 'EB', TABSYM, TABPTR, TABVAL )
!
!     does not modify the contents of the symbol table. It signals
!     the error SPICE(NOSUCHSYMBOL) because the symbol "K" does not
!     exist in the symbol table.
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
!-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
!
!         Declaration of the unused function SIZEC removed.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               BSRCHC
      INTEGER               CARDC
      INTEGER               LSTLEC
      LOGICAL               RETURN
      INTEGER               SUMAI

!
!     Local variables
!
      INTEGER               NSYM

      INTEGER               OLDLOC
      INTEGER               OLDVAL
      INTEGER               OLDDIM
      INTEGER               NEWLOC
      INTEGER               NEWVAL



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SYREND' )
      END IF

!
!     Where was the old symbol?
!
      NSYM   = CARDC  ( TABSYM )
      OLDLOC = BSRCHC ( OLD, NSYM, TABSYM(1) )

!
!     An overflow is simply not possible here. The only thing that can
!     go wrong is that the old symbol does not exist.
!
      IF ( OLDLOC .EQ. 0 ) THEN


         CALL SETMSG ( 'SYREND: The symbol # is not in the symbol ' // &
                       'table.' )
         CALL ERRCH  ( '#', OLD )
         CALL SIGERR ( 'SPICE(NOSUCHSYMBOL)' )

!
!     Are these the same symbol?
!
      ELSE IF ( NEW .NE. OLD ) THEN

!
!        If the new symbol already exists, delete it.
!
         CALL SYDELD ( NEW, TABSYM, TABPTR, TABVAL )

         NSYM   = CARDC  (            TABSYM    )
         OLDLOC = BSRCHC ( OLD, NSYM, TABSYM(1) )

!
!        Swap N elements at the old location with zero elements
!        at the new location.
!
         NEWLOC = LSTLEC ( NEW, NSYM, TABSYM(1) ) + 1

         OLDVAL = SUMAI ( TABPTR(1), OLDLOC-1 ) + 1
         NEWVAL = SUMAI ( TABPTR(1), NEWLOC-1 ) + 1

         OLDDIM = TABPTR(OLDLOC)

         CALL SWAPAD ( OLDDIM, OLDVAL, 0, NEWVAL, TABVAL(1) )

!
!        Move the name and dimension the same way.
!
         CALL SWAPAC ( 1, OLDLOC, 0, NEWLOC, TABSYM(1) )
         CALL SWAPAI ( 1, OLDLOC, 0, NEWLOC, TABPTR(1) )

         IF ( OLDLOC .LT. NEWLOC ) THEN
            NEWLOC = NEWLOC - 1
         END IF

         TABSYM(NEWLOC) = NEW

      END IF


      CALL CHKOUT ( 'SYREND' )
      RETURN
      END
