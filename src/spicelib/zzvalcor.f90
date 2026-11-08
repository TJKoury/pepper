!$Procedure ZZVALCOR ( Validate aberration correction )

      subroutine zzvalcor ( abcorr, attblk )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Validate an aberration correction string suitable for use by
!     the SPK system; return attributes.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ABERRATION
!     PARSING
!     PRIVATE
!     UTILITY
!
! $ Declarations

      implicit none

      include 'zzabcorr.inc'

      character*(*)         abcorr
      logical               attblk ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     ABCORR     I   Aberration correction string.
!     ATTBLK     O   Aberration correction attribute block.
!
! $ Detailed_Input
!
!     ABCORR         is a string representing a aberration
!                    correction.  The supported values are:
!
!                       'CN'
!                       'CN+S'
!                       'LT'
!                       'LT+S'
!                       'NONE'
!                       'XCN'
!                       'XCN+S'
!                       'XLT'
!                       'XLT+S'
!
!                    Note that some values not supported by the
!                    SPICELIB SPK subsystem are supported by
!                    the underlying routine ZZPRSCOR:
!
!                       - The letter 'R' indicates relativistic
!                         corrections.
!
!                       - Stellar aberration-only corrections are
!                         indicated by the strings
!
!                            'S'
!                            'XS'
!
!                    This routine *does not* permit values that
!                    the SPK system doesn't handle.
!
!                    Case and embedded blanks are not significant in
!                    ABCORR.
!
!                    If ABCORR contains an unsupported value, this
!                    routine will signal an error.
!
! $ Detailed_Output
!
!     ATTBLK         is a block of logical flags indicating the
!                    attributes of the aberration correction
!                    specified by ABCORR.  The attributes are:
!
!                       - Is the correction "geometric"?
!
!                       - Is light time correction indicated?
!
!                       - Is stellar aberration correction indicated?
!
!                       - Is the light time correction of the
!                         "converged Newtonian" variety?
!
!                       - Is the correction for the transmission
!                         case?
!
!                       - Is the correction relativistic? (This
!                         value is always .FALSE. for aberration
!                         correction specifications allowed by
!                         this routine.)
!
!                    The structure of ATTBLK is defined in the
!                    include file
!
!                       zzabcorr.inc
!
!                    The size of ATTBLK and the offsets of the
!                    component flags are defined there.
!
! $ Parameters
!
!     See INCLUDE file zzabcorr.inc.
!
! $ Exceptions
!
!     1) If the input aberration correction choice is not allowed,
!        the error SPICE(INVALIDOPTION) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is similar to ZZPRSCOR, but stellar aberration-only
!     and relativistic corrections specifications are not allowed
!     by this routine. The allowed values are precisely those allowed
!     by SPKEZR.
!
! $ Examples
!
!     See ZZGFOCIN.
!
! $ Restrictions
!
!     1) This is a SPICE private routine; the routine is subject
!        to change without notice.  User applications should not
!        call this routine.
!
!     2) This routine recognizes some aberration corrections not
!        handled by most SPICELIB routines.  Callers should do
!        their own checking to ensure the parsed correction is
!        acceptable.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 1.0.0, 11-APR-2008 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed
      logical               return


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZVALCOR' )

!
!     Parse the aberration correction string and obtain
!     an attribute block.
!
      call zzprscor ( abcorr, attblk )

      if ( failed() ) then
         call chkout ( 'ZZVALCOR' )
         return
      end if

!
!     Check the attribute block. We don't allow relativistic
!     corrections.
!
      if ( attblk(relidx) ) then

         call setmsg ( 'Aberration correction specification # calls ' &
         //            'for relativistic corrections, which are ' &
         //            'not supported.'                               )
         call errch  ( '#',  abcorr                                   )
         call sigerr ( 'SPICE(INVALIDOPTION)'                         )
         call chkout ( 'ZZVALCOR'                                     )
         return

      end if

!
!     Stellar aberration corrections are allowed only if light
!     time corrections are specified as well.
!
      if (  attblk(stlidx)  .and.  ( .not. attblk(ltidx) )  ) then

         call setmsg ( 'Aberration correction specification # calls ' &
         //            'for stellar aberration correction without ' &
         //            'light time correction; this combination ' &
         //            'is not supported.'                            )
         call errch  ( '#',  abcorr                                   )
         call sigerr ( 'SPICE(INVALIDOPTION)'                         )
         call chkout ( 'ZZVALCOR'                                     )
         return

      end if

      call chkout ( 'ZZVALCOR' )
      return
      end
