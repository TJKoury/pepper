!$Procedure      F_ASNACSN (Family of tests for DASINE and DACOSN)

      SUBROUTINE F_ASNACSN ( OK )

      IMPLICIT              NONE

      LOGICAL               OK

! $ Abstract
!
!     This is a test routine for the arc sine and arc cosine
!     functions with argument bound checking.
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
! $ Version
!
!     TSPICE Version 1.0.0 7-MAR-2006 (LSE)
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB functions
!

      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      DACOSN
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      HALFPI

!
!     Local variables.
!

      DOUBLE PRECISION      TSTARG
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TSTANG

!
!     Begin every test family with an open call.
!

      CALL TOPEN ( 'F_ASNACSN' )

!
!     Test for tolerance negative error.
!

      TOL       =  -1.D-12

      CALL TCASE  ( 'Tolerance negative, error in DASINE' )

      TSTANG    =   DASINE ( 0.D0,  TOL                   )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK  )

      CALL TCASE  ( 'Tolerance negative, error in DACOSN' )

      TSTANG    =   DACOSN ( 1.D0,  TOL                   )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK  )

!
!     Test for error when argument is too large
!

      TOL       =  1.D-12

      TSTARG    =  1.D0 + 1.D-11

      CALL TCASE  ( 'Argument too large in DASINE.'               )

      TSTANG    =  DASINE ( TSTARG,  TOL                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         )

      CALL TCASE  ( 'Is value = ASIN (1.0) in DASINE?)'           )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=', HALFPI(),  0.D0,  OK )

      CALL TCASE  ( 'Argument too large in DACOSN.'               )

      TSTANG    =  DACOSN ( TSTARG,  TOL                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         )

      CALL TCASE  ( 'Is value = ACOS (1.0) in DACOSN?)'           )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=', 0.D0,  0.D0,  OK     )

!
!     Test for error when argument is too small
!

      TOL       =  1.D-12

      TSTARG    = -1.D0 - 1.D-11

      CALL TCASE  ( 'Argument too small in DASINE.'               )

      TSTANG    =  DASINE ( TSTARG,  TOL                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         )

      CALL TCASE  ( 'Is value = ASIN (-1.0) in DASINE?)'          )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=',-HALFPI(),  0.D0,  OK )

      CALL TCASE  ( 'Argument too small in DACOSN.'               )

      TSTANG    =  DACOSN ( TSTARG,  TOL                          )

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         )

      CALL TCASE  ( 'Is value = ACOS (-1.0) in DACOSN?)'          )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=', PI(),  0.D0,  OK     )

!
!     All done.
!

      CALL T_SUCCESS ( OK )
      RETURN
      END

