!$Procedure      F_ZZVALCOR ( Test ZZVALCOR )

      SUBROUTINE F_ZZVALCOR ( OK )

! $ Abstract
!
!     Test the aberration correction parsing routine ZZVALCOR.
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

      INCLUDE              'zzabcorr.inc'

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
!     This family tests the aberration correction parsing
!     utility routine ZZVALCOR.
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
!-    SPICELIB Version 1.0.0, 26-FEB-2009 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               MATCHI

!
!     Local parameters
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXCOR
      PARAMETER           ( MAXCOR = 9 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    CORLST ( MAXCOR )
      CHARACTER*(LNSIZE)    QNAME

      INTEGER               I
      INTEGER               J

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               XATBLK ( ABATSZ )

!
!     Saved everything
!
      SAVE

!
!     Initial values
!
      DATA                 CORLST /  'None', &
                                     'lT', &
                                     'Xlt', &
                                     'lt + s', &
                                     'XLt + S', &
                                     'Cn', &
                                     'xCn', &
                                     'cN  +  s', &
                                     'xCN +  S'   /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZVALCOR' )




!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Reject stellar aberration correction ' &
      //           'without light time correction'         )


      CALL ZZVALCOR ( 'S', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( ' s', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( '+s', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( ' + s', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( ' + S', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )



!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'Reject relativistic light time correction' )


      CALL ZZVALCOR ( 'RLT', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( ' rlT', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( 'xRLT', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( ' XrlT', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( 'RLT + S', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL ZZVALCOR ( 'xRLT + s', ATTBLK )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )




!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************






!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Check attribute block for each normal ' &
      //           'aberration correction flag.'            )


      DO I = 1, MAXCOR
!
!        Parse the correction.
!
         CALL ZZVALCOR ( CORLST(I), ATTBLK )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Set up the expected attribute block.
!
         DO J = 1, ABATSZ
            XATBLK(J) = .FALSE.
         END DO

!
!        Set the expected geometric element.
!
         XATBLK( GEOIDX )  =  MATCHI( CORLST(I), '*NONE*', '*', '?' )

!
!        Set the expected transmission element.
!
         XATBLK( XMTIDX )  =  MATCHI( CORLST(I), '*X*', '*', '?' )

!
!        Set the expected stellar aberration element.
!
         XATBLK( STLIDX )  =  MATCHI( CORLST(I), '*+*S*', '*', '?' )

!
!        Set the expected relativistic correction element.
!
         XATBLK( RELIDX )  =  MATCHI( CORLST(I), '*RLT*', '*', '?' )

!
!        Set the expected light time and converted Newtonian
!        elements.
!
         IF (   MATCHI( CORLST(I), '*LT*', '*', '?' )   ) THEN
!
!           The light time element should be set; the converged
!           Newtonian element should not.
!
            XATBLK( LTIDX  ) = .TRUE.
            XATBLK( CNVIDX ) = .FALSE.

         ELSE IF (   MATCHI( CORLST(I), '*CN*', '*', '?' )   ) THEN

            XATBLK( LTIDX  ) = .TRUE.
            XATBLK( CNVIDX ) = .TRUE.

         ELSE

            XATBLK( LTIDX  ) = .FALSE.
            XATBLK( CNVIDX ) = .FALSE.

         END IF


!
!        Check each element of the attribute block.
!
         DO J = 1, ABATSZ

            QNAME = CORLST(I)
            CALL SUFFIX ( '/',             0, QNAME )
            CALL SUFFIX ( 'attblk elt. #', 1, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPMI ( QNAME, '#', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( QNAME, ATTBLK(J), XATBLK(J), OK )


         END DO

      END DO

      RETURN
      END





