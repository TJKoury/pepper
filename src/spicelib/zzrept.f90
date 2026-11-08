!$Procedure      ZZREPT ( Private --- replace tokens )

      logical function zzrept ( sub, replac, l2r )

! $ Abstract
!
!    SPICE Private routine intended solely for the support of SPICE
!    routines.  Users should not call this routine directly due
!    to the volatile nature of this routine.
!
!    Replace matching tokens and remove the character "*"
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TIME --- PRIVATE
!
! $ Declarations

      implicit none
      character*(*)         sub
      character*(*)         replac
      logical               l2r

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SUB        I   is the substring to perform replacements on
!     REPLAC     I   is the replacement string
!     L2R        I   use left to right scanning if L2R is TRUE.
!
!     The function returns TRUE if a replacement is performed
!
! $ Detailed_Input
!
!     SUB        is a substring of characters to located in the
!                current internal tokenized representation of a
!                time string that is maintained by ZZTIME.
!
!     REPLAC     is a string of characters that will replace 1 for 1
!                the characters in SUB.  Note that character * is
!                a special character in this substitution as it
!                will be removed (via ZZREMT) after substitution.
!
!     L2R        is a logical flag.  If L2R is TRUE, the search
!                for a substring matching SUB will be performed
!                in left to right order. If L2R is FALSE the
!                search for substring matching SUB will be performed
!                from right to left.
!
! $ Detailed_Output
!
!     The function returns TRUE if a replacement is performed. Otherwise
!     it returns FALSE.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This routine is simply a macro for the combination of the
!     ZZTIME entry points ZZSUBT and ZZREMT
!
! $ Examples
!
!     See TPARTV.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 8-APR-1996 (WLT)
!
!
!-&


      logical               zzremt
      logical               zzsubt

      logical               ok

      zzrept = zzsubt ( sub, replac, l2r )
      ok     = zzremt ( '*' )

      return
      end
