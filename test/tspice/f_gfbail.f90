!$Procedure      F_GFBAIL ( Test GFBAIL )

      SUBROUTINE F_GFBAIL ( OK )

! $ Abstract
!
!     Test the GF stub interrupt indicator routine GFBAIL.
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
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE


      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This family tests the GF stub interrupt indicator routine GFBAIL.
!
!     Note that since ANSI standard Fortran 77 doesn't support
!     interrupt handling, GFBAIL is almost a "no-op" routine.
!     The action of GFBAIL is to return the logical value .FALSE.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 02-AUG-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               GFBAIL

!
!     Local parameters
!


!
!     Local Variables
!
      LOGICAL               STATUS

!
!     Saved everything.
!
      SAVE

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFBAIL' )






!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Make sure function returns FALSE on initial call.' )

      STATUS = .TRUE.
      STATUS = GFBAIL()

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'STATUS', STATUS, .FALSE., OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Make sure function returns FALSE on 2nd call.' )

      STATUS = .TRUE.
      STATUS = GFBAIL()

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'STATUS', STATUS, .FALSE., OK )


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test routine''s action when a SPICE error ' &
      //           'condition exists.'                          )

      CALL SIGERR ( 'SPICE(TESTERROR)' )

      STATUS = .TRUE.
      STATUS = GFBAIL()

      CALL CHCKXC ( .TRUE., 'SPICE(TESTERROR)', OK )

      CALL CHCKSL ( 'STATUS', STATUS, .FALSE., OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END




