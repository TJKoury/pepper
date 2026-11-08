!$Procedure F_T_URAND ( T_URAND tests )

      SUBROUTINE F_T_URAND ( OK )

! $ Abstract
!
!     Exercise T_URAND, the SPICE test utility routine that generates
!     uniformly distributed random numbers between 0 and 1.
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
!     TEST FAMILY
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
!     This routine tests the test utility routine T_URAND.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 17-OCT-2011 (NJB)
!
!
!-&

!
!     SPICELIB functions
!

!
!     Other functions
!
      DOUBLE PRECISION      T_URAND
      DOUBLE PRECISION      T_URAND2

!
!     Local Parameters
!

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 800 )

      INTEGER               NRANDM
      PARAMETER           ( NRANDM = 2000 )


!
!     Local Variables
!
      CHARACTER*(LNSIZE)    NAME
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      U
      DOUBLE PRECISION      U2

      INTEGER               I
      INTEGER               N
      INTEGER               SEED
      INTEGER               SEED2

!
!     Saved values
!

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_T_URAND' )


!
! --- Case: ------------------------------------------------------
!
!
      TITLE = 'Generate a series of random numbers, ' &
      //      'starting with an initializing seed.'
      CALL TCASE ( TITLE )

      N = 100000

      SEED  = -1
      SEED2 = -1


      DO I = 1, N

         U = T_URAND ( SEED )
         CALL CHCKXC ( .FALSE., ' ', OK )

         U2 = T_URAND2 ( SEED2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         NAME = 'Case 1 iteration #.'
         CALL REPMI ( NAME, '#', I, NAME )

         CALL CHCKSI ( NAME, U, '=', U2, 0, OK )

!         IF ( U .LT. 1.D-3 ) THEN
!            WRITE (*,*) 'U  = ', U
!            WRITE (*,*) 'U2 = ', U2
!            WRITE (*,*) ' '
!         END IF

      END DO

      RETURN
      END
