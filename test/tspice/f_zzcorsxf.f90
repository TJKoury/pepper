!$Procedure      F_ZZCORSXF ( Test ZZCORSXF )

      SUBROUTINE F_ZZCORSXF ( OK )

! $ Abstract
!
!     Test the SPICELIB private state transformation correction
!     routine ZZCORSXF.
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

      INCLUDE               'gf.inc'
      INCLUDE               'zzabcorr.inc'

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
!     This routine tests the private SPICELIB state
!     transformation correction routine ZZCORSXF.
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
!-    SPICELIB Version 1.0.0, 22-MAY-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!

!
!     Local parameters
!
!
!     Local parameters
!

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )


!
!     Local variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      CORXFM ( 6, 6 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      LTSIGN
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XCORXF ( 6, 6 )

      INTEGER               I
      INTEGER               J
      INTEGER               K

      LOGICAL               XMIT

!
!     Save everything.
!
      SAVE

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZCORSXF' )


!
!     ZZCORSXF is error free,so there are no error cases to
!     check.
!

!
!     Loop over the radiation direction cases.
!
      DLT = 5.D-2

      DO I = 1, 2
!
!---- Case -------------------------------------------------------------
!
         IF ( I .EQ. 1 ) THEN

            XMIT   = .TRUE.
            LTSIGN = 1.D0
            TITLE  = 'Initial matrix filled with index ' &
            //       'values. XMIT = .TRUE.'
         ELSE
            XMIT   = .FALSE.
            LTSIGN = -1.D0
            TITLE  = 'Initial matrix filled with index ' &
            //       'values. XMIT = .FALSE.'
         END IF

         CALL TCASE ( TITLE )

!
!        Since ZZCORSXF doesn't check the input matrix, we don't
!        have to use a real state transformation. Instead, simply
!        fill the original matrix with identifiable, distinct
!        values.
!
         DO J = 1, 6

            DO K = 1, 6
               XFORM( K, J ) = (J-1)*6 + K
            END DO

         END DO

!
!        Form the expected, transformed matrix.
!
         CALL MOVED ( XFORM, 36, XCORXF )

         DO J = 1, 3

            DO K = 4, 6
               XCORXF( K, J ) = XFORM(K,J) * ( 1.D0 + LTSIGN*DLT )
            END DO

         END DO

         CALL ZZCORSXF ( XMIT, DLT, XFORM, CORXFM )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'CORXFM', CORXFM, '~', XCORXF, 36, VTIGHT, OK )

      END DO

      CALL T_SUCCESS ( OK )

      RETURN
      END




