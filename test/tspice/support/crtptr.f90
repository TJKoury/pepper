!$Procedure                  CRTPTR (Create pointer)

      CHARACTER*(*) FUNCTION CRTPTR (BASE, INDEX, PNTER)

! $ Abstract
!
!     Returns the symbol 'BASE~INDEX~PNTER'.
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
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations

      CHARACTER*(*)         BASE
      INTEGER               INDEX
      CHARACTER*(*)         PNTER

      INTEGER               SYMLEN
      PARAMETER           ( SYMLEN = 1024 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!      BASE,
!      INDEX,
!      PNTER     I    Components of the symbol 'BASE~INDEX~PNTER'.
!
!      SYMLEN    P    Maximum length of the symbol.
!
! $ Detailed_Input
!
!     BASE,
!     INDEX,
!     PNTER      are components of the symbol 'BASE~INDEX~PNTER'.
!
! $ Detailed_Output
!
!     CRTPTR     is the symbol 'BASE~INDEX~PNTER'.
!
! $ Parameters
!
!     SYMLEN     is the maximum length of the symbol 'BASE~INDEX~PNTER'.
!
! $ Exceptions
!
!     1) If the length of the symbol 'BASE~INDEX~PNTER' exceeds SYMLEN,
!        the error SPICE(BUFFERTOOSMALL) is signalled.
!
!     2) If the length of the symbol 'BASE~INDEX~PNTER' exceeds
!        LEN(CRTPTR), the error SPICE(DIMENSIONTOOSMALL) is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine creates a symbol that may be used to look up
!     nodes in the symbol table created by CPARSE.
!
! $ Examples
!
!     None.
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
!     M.J. Spencer   (JPL)
!
! $ Version
!
!-    Version 1.1.1, 13-JAN-2007, (EDW)
!
!        Corrected typo in the previous version string;
!        from:
!
!           09-DEC-203
!
!        to
!
!           09-DEC-2003
!
!-    Version 1.1.0, 09-DEC-2003, (EDW)
!
!       Set the SYMLEN value to 1024 to match the same
!       value in niospk.
!
!-    Beta Version 1.0.0, 11-AUG-1992 (MJS)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               RTRIM
      LOGICAL               RETURN

!
!     Local variables
!
      CHARACTER*(10)        CNUM
      CHARACTER*(SYMLEN)    SYM

      INTEGER               BLEN
      INTEGER               CLEN
      INTEGER               PLEN
      INTEGER               TOTAL

!
!     This routine will use discovery check-in.
!
      IF (RETURN()) THEN
         RETURN
      END IF

!
!     Compute the lengths of the strings involved.
!
      CALL INTSTR (INDEX, CNUM)

      CLEN  = RTRIM (CNUM)
      BLEN  = RTRIM (BASE)
      PLEN  = RTRIM (PNTER)
      TOTAL = CLEN+BLEN+PLEN+2

!
!     TOTAL must be SYMLEN characters, or fewer.
!
      IF (TOTAL .GT. SYMLEN) THEN
         CALL CHKIN  ('CRTPTR'                                     )
         CALL SETMSG ('Symbol exceeds # characters. Increase the '  // &
                      'value of SYMLEN.'                           )
         CALL ERRINT ('#', SYMLEN                                  )
         CALL SIGERR ('SPICE(BUFFERTOOSMALL)'                      )
         CALL CHKOUT ('CRTPTR'                                     )
         RETURN
      END IF

!
!     And TOTAL must be LEN(CRTPTR) characters, or fewer.
!
      IF (TOTAL .GT. LEN(CRTPTR)) THEN
         CALL CHKIN  ('CRTPTR'                                 )
         CALL SETMSG ('Symbol exceeds the dimension of CRTPTR.')
         CALL SIGERR ('SPICE(DIMENSIONTOOSMALL)'               )
         CALL CHKOUT ('CRTPTR'                                 )
         RETURN
      END IF

!
!     Form the symbol 'BASE~INDEX~PNTER'.
!
      SYM = ' '
      SYM = BASE(1:BLEN) //'~'// CNUM(1:CLEN) //'~'// PNTER(1:PLEN)

      CRTPTR = SYM

      RETURN
      END
