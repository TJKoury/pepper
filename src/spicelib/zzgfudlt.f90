!$Procedure ZZGFUDLT ( Private --- GF, scalar function < ref value )

      SUBROUTINE ZZGFUDLT ( UDFUNC, ET, ISLESS )

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     This routine determines if the value of the scalar quantity
!     function is less than a previously defined reference value.
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
!     GEOMETRY
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'zzholdd.inc'

      EXTERNAL              UDFUNC

      DOUBLE PRECISION      ET
      LOGICAL               ISLESS

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ZZGET      P   ZZHOLDD retrieves a stored DP value.
!     GF_REF     P   ZZHOLDD acts on the reference value
!                    for a GF search.
!     UDFUNC     I   Name of the routine that computes the scalar value
!                    of interest at ET.
!     ET         I   Time in TDB seconds for which to evaluate UDFUNC.
!     ISLESS     O   Boolean indicating if the scalar value is less than
!                    reference value.
!
! $ Detailed_Input
!
!     UDFUNC     the routine that returns the value of the scalar
!                quantity of interest at time ET. The calling sequence
!                for UDFUNC is:
!
!                   CALL UDFUNC ( ET, VALUE )
!
!                where:
!
!                   ET      a double precision value representing
!                           ephemeris time, expressed as seconds past
!                           J2000 TDB, at which to determine the scalar
!                           value.
!
!                   VALUE   is the value of the geometric quantity
!                           at ET.
!
!     ET         a double precision value representing ephemeris time,
!                expressed as seconds past J2000 TDB at which to
!                determine the value of UDFUNC.
!
! $ Detailed_Output
!
!     ISLESS     a scalar boolean indicating if the value of UDFUNC at
!                ET is less than REFVAL (true) or not (false).
!
!                 Functionally:
!
!                   ISLESS = UDFUNC( ET )  <  REFVAL
!
! $ Parameters
!
!    None.
!
! $ Exceptions
!
!     1) ZZHOLDD will signal the error SPICE(ZZHOLDNOPUT) if this
!        routine is called prior to storing a reference value
!        using a ZZHOLDD "PUT" operation.
!
! $ Files
!
!    None.
!
! $ Particulars
!
!     A ZZHOLDD "PUT" stored the reference value used in the logical
!     operation. A ZZHOLDD "GET" retrieves the value.
!
! $ Examples
!
!    See GFUDS.
!
! $ Restrictions
!
!    None.
!
! $ Literature_References
!
!    None.
!
! $ Author_and_Institution
!
!    N.J. Bachman   (JPL)
!    E.D. Wright    (JPL)
!
! $ Version
!
!-   SPICELIB Version 1.1.0, 06-AUG-2010 (EDW)
!
!       Change in ZZHOLDD functionality required edit to ZZHOLDD
!       call and inclusion of "zzholdd.inc" parameters file.
!
!-   SPICELIB Version 1.0.0, 16-FEB-2010 (EDW)
!
!-&

! $ Index_Entries
!
!    function less than reference value
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN
      LOGICAL               FAILED

!
!     Local Variables
!
      DOUBLE PRECISION      UDVAL
      DOUBLE PRECISION      REFVAL

      LOGICAL               OK


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFUDLT' )

!
!     Default value of false for ISLESS.
!
      ISLESS = .FALSE.

!
!     Call the routine, return the scalar value corresponding to ET.
!
      CALL UDFUNC ( ET, UDVAL )

!
!     Check for an error, return if found.
!
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFUDLT' )
         RETURN
      END IF

!
!     Retrieve the stored reference value. Signal an error if a
!     get call fails.
!
      CALL ZZHOLDD ( ZZGET, GF_REF, OK, REFVAL )

      IF ( .NOT. OK ) THEN
         CALL SETMSG ( 'ZZHOLDD GET failed. This indicates a logic ' &
                 //    'error in the GF code due either to a failure ' &
                 //    'to store the GF reference value or a post ' &
                 //    'store reset of ZZHOLDD.' )
         CALL SIGERR ( 'SPICE(ZZHOLDDGETFAILED)' )
         CALL CHKOUT ( 'ZZGFUDLT'                )
         RETURN
      END IF

      ISLESS = UDVAL .LT. REFVAL

      CALL CHKOUT ( 'ZZGFUDLT' )
      RETURN

      END



