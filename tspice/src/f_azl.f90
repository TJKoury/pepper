!$Procedure F_AZL ( Azimuth/elevation coordinate tests )

      SUBROUTINE F_AZL ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB azimuth/elevation coordinate
!     conversion and Jacobian routines.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB azimuth/elevation coordinate
!     and coordinate derivative conversion routines
!
!        AZLREC
!        DAZLDR
!        DRDAZL
!        RECAZL
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 22-DEC-2020 (JDR) (NJB)
!
!-&


! $ Index_Entries
!
!     Test family for AZ/EL coordinate routines
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      T_RANDD
      DOUBLE PRECISION      TWOPI

      LOGICAL               VZERO

!
!     Local parameters
!
      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE = 1.D-7 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NCASE
      PARAMETER           ( NCASE  = 28 )

      INTEGER               NFLAGS
      PARAMETER           ( NFLAGS = 4  )

      INTEGER               NVECT
      PARAMETER           ( NVECT  = 7  )

      INTEGER               NSCALE
      PARAMETER           ( NSCALE = 5 )

      INTEGER               NRAND
      PARAMETER           ( NRAND  = 100 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      AZ
      DOUBLE PRECISION      AZ2
      DOUBLE PRECISION      DELTAZ
      DOUBLE PRECISION      DXDAZ  ( NCASE )
      DOUBLE PRECISION      DXDEL  ( NCASE )
      DOUBLE PRECISION      DXDR   ( NCASE )
      DOUBLE PRECISION      DYDAZ  ( NCASE )
      DOUBLE PRECISION      DYDEL  ( NCASE )
      DOUBLE PRECISION      DYDR   ( NCASE )
      DOUBLE PRECISION      DZDAZ  ( NCASE )
      DOUBLE PRECISION      DZDEL  ( NCASE )
      DOUBLE PRECISION      DZDR   ( NCASE )
      DOUBLE PRECISION      EL2
      DOUBLE PRECISION      E3     ( 3 )
      DOUBLE PRECISION      EL
      DOUBLE PRECISION      IDMAT  ( 3, 3 )
      DOUBLE PRECISION      INJACO ( 3, 3 )
      DOUBLE PRECISION      JAC2   ( 3, 3 )
      DOUBLE PRECISION      JACOBI ( 3, 3 )
      DOUBLE PRECISION      MPROD  ( 3, 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      REC    ( 3 )
      DOUBLE PRECISION      REC2   ( 3 )
      DOUBLE PRECISION      RECTAN ( 3, NVECT )
      DOUBLE PRECISION      SCALES ( NSCALE )
      DOUBLE PRECISION      XAZ    ( NCASE )
      DOUBLE PRECISION      XEL    ( NCASE )
      DOUBLE PRECISION      XEL2
      DOUBLE PRECISION      XR     ( NCASE )
      DOUBLE PRECISION      XRANGE
      DOUBLE PRECISION      XREC   ( 3 )


      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               M
      INTEGER               SEED

      LOGICAL               AZCCW  ( NFLAGS )
      LOGICAL               ELPLSZ ( NFLAGS )

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                 E3 / 0.D0, 0.D0, 1.D0 /

      DATA                ( ( RECTAN(J,I), J = 1, 3 ), I = 1, NVECT )  / &
                                     1.000D0,     0.000D0,     0.000D0, &
                                    -1.000D0,     0.000D0,     0.000D0, &
                                     0.000D0,     1.000D0,     0.000D0, &
                                     0.000D0,    -1.000D0,     0.000D0, &
                                     0.000D0,     0.000D0,     1.000D0, &
                                     0.000D0,     0.000D0,    -1.000D0, &
                                     0.000D0,     0.000D0,     0.000D0 &
                                    /

      DATA                 ( AZCCW(I), ELPLSZ(I), I = 1, 4 ) / &
                               .TRUE.,    .TRUE., &
                               .TRUE.,   .FALSE., &
                              .FALSE.,    .TRUE., &
                              .FALSE.,   .FALSE. &
                             /

      DATA       ( XR(I),     XAZ(I),     XEL(I), I = 1, 19 ) / &
                 1.000D0,    0.000D0,    0.000D0, &
                 1.000D0,    0.000D0,    0.000D0, &
                 1.000D0,    0.000D0,    0.000D0, &
                 1.000D0,    0.000D0,    0.000D0, &
                 1.000D0,  180.000D0,    0.000D0, &
                 1.000D0,  180.000D0,    0.000D0, &
                 1.000D0,  180.000D0,    0.000D0, &
                 1.000D0,  180.000D0,    0.000D0, &
                 1.000D0,   90.000D0,    0.000D0, &
                 1.000D0,   90.000D0,    0.000D0, &
                 1.000D0,  270.000D0,    0.000D0, &
                 1.000D0,  270.000D0,    0.000D0, &
                 1.000D0,  270.000D0,    0.000D0, &
                 1.000D0,  270.000D0,    0.000D0, &
                 1.000D0,   90.000D0,    0.000D0, &
                 1.000D0,   90.000D0,    0.000D0, &
                 1.000D0,    0.000D0,   90.000D0, &
                 1.000D0,    0.000D0,  -90.000D0, &
                 1.000D0,    0.000D0,   90.000D0 /

      DATA       ( XR(I),     XAZ(I),     XEL(I), I = 20, NCASE ) / &
                 1.000D0,    0.000D0,  -90.000D0, &
                 1.000D0,    0.000D0,  -90.000D0, &
                 1.000D0,    0.000D0,   90.000D0, &
                 1.000D0,    0.000D0,  -90.000D0, &
                 1.000D0,    0.000D0,   90.000D0, &
                 0.000D0,    0.000D0,    0.000D0, &
                 0.000D0,    0.000D0,    0.000D0, &
                 0.000D0,    0.000D0,    0.000D0, &
                 0.000D0,    0.000D0,    0.000D0 /

!
!     The equations for the transformation between rectangular and
!     azimuth/elevation coordinates, taking into account the possible
!     ways of measuring azimuth and elevation are as follows:
!
!        X = R * DCOS( AZSNSE * AZ ) * DCOS( ELDIR * EL )
!        Y = R * DSIN( AZSNSE * AZ ) * DCOS( ELDIR * EL )
!        Z = R *                       DSIN( ELDIR * EL )
!
!     or equivalently, since cosine is an even function,
!
!        X = R * DCOS( AZ )          * DCOS( EL )
!        Y = R * DSIN( AZSNSE * AZ ) * DCOS( EL )
!        Z = R * DSIN( ELDIR  * EL )
!
!     The following data block contains test values for
!
!        DX/DR  =               DCOS( AZSNSE * AZ ) * DCOS( ELDIR * EL )
!        DX/DAZ = -R * AZSNSE * DSIN( AZSNSE * AZ ) * DCOS( ELDIR * EL )
!        DX/DEL = -R * ELDIR  * DCOS( AZSNSE * AZ ) * DSIN( ELDIR * EL )
!
      DATA      ( DXDR(I),   DXDAZ(I),  DXDEL(I), I = 1, 19 ) / &
                  1.000D0,    0.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                  0.000D0,   -1.000D0,   0.000D0, &
                  0.000D0,   -1.000D0,   0.000D0, &
                  0.000D0,    1.000D0,   0.000D0, &
                  0.000D0,    1.000D0,   0.000D0, &
                  0.000D0,    1.000D0,   0.000D0, &
                  0.000D0,    1.000D0,   0.000D0, &
                  0.000D0,   -1.000D0,   0.000D0, &
                  0.000D0,   -1.000D0,   0.000D0, &
                  0.000D0,    0.000D0,  -1.000D0, &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,  -1.000D0  /

      DATA      ( DXDR(I),   DXDAZ(I),  DXDEL(I), I = 20, NCASE ) / &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,  -1.000D0, &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,  -1.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0 /

!
!     The following data block contains test values for
!
!        DY/DR  =               DSIN( AZSNSE * AZ ) * DCOS( ELDIR * EL )
!        DY/DAZ =  R * AZSNSE * DCOS( AZSNSE * AZ ) * DCOS( ELDIR * EL )
!        DY/DEL = -R * ELDIR  * DSIN( AZSNSE * AZ ) * DSIN( ELDIR * EL )
!
!     or equivalently, since cosine is an even function,
!
!        DY/DR  =  DSIN( AZSNSE * AZ ) * DCOS( EL )
!        DY/DAZ =  R * AZSNSE * DCOS( AZ ) * DCOS( EL )
!        DY/DEL = -R * ELDIR  * DSIN( AZSNSE * AZ ) * DSIN( ELDIR * EL )

      DATA      ( DYDR(I),   DYDAZ(I),  DYDEL(I), I = 1, 19 ) / &
                  0.000D0,    1.000D0,   0.000D0, &
                  0.000D0,    1.000D0,   0.000D0, &
                  0.000D0,   -1.000D0,   0.000D0, &
                  0.000D0,   -1.000D0,   0.000D0, &
                  0.000D0,   -1.000D0,   0.000D0, &
                  0.000D0,   -1.000D0,   0.000D0, &
                  0.000D0,    1.000D0,   0.000D0, &
                  0.000D0,    1.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0  /

      DATA      ( DYDR(I),   DYDAZ(I),  DYDEL(I), I = 20, NCASE ) / &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0  /

!
!     The following data block contains test values for
!
!        DZ/DR  =              DSIN( ELDIR * EL )
!        DZ/DAZ =              0.000D0
!        DZ/DEL = R * ELDIR  * DCOS( ELDIR * EL )
!
      DATA      ( DZDR(I),   DZDAZ(I),  DZDEL(I), I = 1, 19 ) / &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,  -1.000D0, &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,  -1.000D0, &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,  -1.000D0, &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,  -1.000D0, &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,  -1.000D0, &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,  -1.000D0, &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,  -1.000D0, &
                  0.000D0,    0.000D0,   1.000D0, &
                  0.000D0,    0.000D0,  -1.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0, &
                  1.000D0,    0.000D0,   0.000D0 /

      DATA      ( DZDR(I),   DZDAZ(I),  DZDEL(I), I = 20, NCASE ) / &
                  1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                 -1.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0, &
                  0.000D0,    0.000D0,   0.000D0 &
                  /

!
!     Scales used for range values:
!
      DATA SCALES / 1.0D-300, 0.5D0, 1.D0, 2.D0, 1.0D300 /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_AZL' )


!
!     *****************************************************************
!
!     Normal cases: RECAZL
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Loop through the canned cases. These cover the nominal
!     use of the routine.
!
      DO I = 1, NVECT

         DO J = 1, NFLAGS

            DO K = 1, NSCALE
!
!     --- Case: -------------------------------------------------------
!
               TITLE = 'Canned case # (RECAZL); AZCCW = #; ELPLSZ = #, ' &
               //      'SCALE = #.'
               CALL REPMI ( TITLE, '#', (NFLAGS * (I-1) +J), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', AZCCW(J),  'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', ELPLSZ(J), 'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', SCALES(K), 6, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               CALL VSCL ( SCALES(K), RECTAN(1,I), REC )

               CALL RECAZL ( REC, AZCCW(J), ELPLSZ(J), R, AZ, EL )
               CALL CHCKXC ( .FALSE., ' ', OK )

               AZ = DPR() * AZ
               EL = DPR() * EL

               XRANGE = SCALES(K) * XR( NFLAGS*(I-1) + J )

               IF ( XRANGE .EQ. 0.D0 ) THEN
                  CALL CHCKSD ( 'RANGE',  R, '~',  XRANGE, VTIGHT, OK )
               ELSE
                  CALL CHCKSD ( 'RANGE',  R, '~/', XRANGE, VTIGHT, OK )
               END IF

               CALL CHCKSD ( 'AZ',    AZ, '~', XAZ( NFLAGS*(I-1) + J ), &
                             VTIGHT, OK )

               CALL CHCKSD ( 'EL',    EL, '~', XEL( NFLAGS*(I-1) + J ), &
                             VTIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!

!
!              Update the title from the previous case.
!
               CALL REPMC ( TITLE, 'Canned', 'Perturbed AZ', TITLE )

               CALL TCASE( TITLE )

               CALL VSCL ( SCALES(K), RECTAN(1,I), REC )

!
!              Perturb the input vector as long as we don't have a
!              degenerate case.
!
               IF (      ( REC(1) .NE. 0.D0 ) &
                    .OR. ( REC(2) .NE. 0.D0 )  ) THEN
!
!                 Rotate the input vector by +30 degrees in the
!                 positive AZ direction.
!
                  DELTAZ = RPD() * 30
               ELSE
                  DELTAZ = 0.D0
               END IF

               IF ( AZCCW(J) ) THEN
                  CALL VROTV ( REC, E3,  DELTAZ, REC2 )
               ELSE
                  CALL VROTV ( REC, E3, -DELTAZ, REC2 )
               END IF

               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL RECAZL ( REC2, AZCCW(J), ELPLSZ(J), R, AZ, EL )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Subtract the delta from the returned azimuth for
!              comparison against the canned expected result.
!
               AZ = AZ - DELTAZ

!
!              Convert AZ and EL to degrees for comparison with expected
!              values.
!
               AZ = DPR() * AZ

               EL = DPR() * EL

!
!              Move AZ into the range [ XAZ - 360, XAZ + 360 ]
!              if necessary. We presume AZ and XAZ differ by a
!              very small amount.
!
               IF ( AZ .LT. ( XAZ( NFLAGS*(I-1)+J ) - 90.D0 ) ) THEN
                  AZ = AZ + 360.D0
               END IF

               IF ( AZ .GT. ( XAZ( NFLAGS*(I-1)+J ) + 90.D0 ) ) THEN
                  AZ = AZ - 360.D0
               END IF

               XRANGE = SCALES(K) * XR( NFLAGS*(I-1) + J )

               IF ( XRANGE .EQ. 0.D0 ) THEN
                  CALL CHCKSD ( 'RANGE',  R, '~',  XRANGE, VTIGHT, OK )
               ELSE
                  CALL CHCKSD ( 'RANGE',  R, '~/', XRANGE, VTIGHT, OK )
               END IF
!
!              Note slightly relaxed tolerance for this case.
!
               CALL CHCKSD ( 'AZ',    AZ, '~', XAZ( NFLAGS*(I-1) + J ), &
                             TIGHT, OK )

               CALL CHCKSD ( 'EL',    EL, '~', XEL( NFLAGS*(I-1) + J ), &
                             VTIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!

!
!              Update the title from the previous case.
!
               CALL REPMC ( TITLE, 'Perturbed AZ', &
                                   'Perturbed AZ and EL', TITLE )

               CALL TCASE( TITLE )

               CALL VSCL ( SCALES(K), RECTAN(1,I), REC )

!
!              Perturb AZ of the input vector as long as we don't have
!              a degenerate case.
!
               IF (      ( REC(1) .NE. 0.D0 ) &
                    .OR. ( REC(2) .NE. 0.D0 )  ) THEN
!
!                 Rotate the input vector by +30 degrees in the
!                 positive AZ direction.
!
                  DELTAZ = RPD() * 30
               ELSE
                  DELTAZ = 0.D0
               END IF

               IF ( AZCCW(J) ) THEN
                  CALL VROTV ( REC, E3,  DELTAZ, REC2 )
               ELSE
                  CALL VROTV ( REC, E3, -DELTAZ, REC2 )
               END IF

!
!              Set EL of the input vector if the original EL is
!              0 degrees but the input is not the zero vector.
!
               IF (       (  RECTAN(3,I) .EQ. 0.D0       ) &
                    .AND. (  .NOT. VZERO( RECTAN(1,I) )  ) ) THEN
!
!                 Set the Z component of REC2 to be consistent with
!                 an elevation magnitude of 60 degrees.
!
                  IF ( ELPLSZ(J) ) THEN
                     REC2(3) =  ( SQRT(3.D0)/2 ) * SCALES(K)
                  ELSE
                     REC2(3) = -( SQRT(3.D0)/2 ) * SCALES(K)
                  END IF
!
!                 Scale the X and Y components of REC2 to be consistent
!                 with the elevation.
!
                  REC2(1) = REC2(1) / 2
                  REC2(2) = REC2(2) / 2

               END IF


               CALL RECAZL ( REC2, AZCCW(J), ELPLSZ(J), R, AZ, EL )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Subtract the delta from the returned azimuth for
!              comparison against the canned expected result.
!
               AZ = AZ - DELTAZ

!
!              Convert AZ and EL to degrees for comparison with expected
!              values.
!
               AZ = DPR() * AZ
               EL = DPR() * EL

!
!              Move AZ into the range [ XAZ - 360, XAZ + 360 ]
!              if necessary. We presume AZ and XAZ differ by a
!              very small amount.
!
               IF ( AZ .LT. ( XAZ( NFLAGS*(I-1)+J ) - 90.D0 ) ) THEN
                  AZ = AZ + 360.D0
               END IF

               IF ( AZ .GT. ( XAZ( NFLAGS*(I-1)+J ) + 90.D0 ) ) THEN
                  AZ = AZ - 360.D0
               END IF


               XRANGE = SCALES(K) * XR( NFLAGS*(I-1) + J )

               IF ( XRANGE .EQ. 0.D0 ) THEN
                  CALL CHCKSD ( 'RANGE',  R, '~',  XRANGE, VTIGHT, OK )
               ELSE
                  CALL CHCKSD ( 'RANGE',  R, '~/', XRANGE, VTIGHT, OK )
               END IF
!
!              Note slightly relaxed tolerance for this case.
!
               CALL CHCKSD ( 'AZ',    AZ, '~', XAZ( NFLAGS*(I-1) + J ), &
                             TIGHT, OK )

               IF (       (  RECTAN(3,I) .EQ. 0.D0       ) &
                    .AND. (  .NOT. VZERO( RECTAN(1,I) )  ) ) THEN

                  XEL2 = 60.D0

                  CALL CHCKSD ( 'EL', EL, '~', XEL2, VTIGHT, OK )

               ELSE

                  CALL CHCKSD ( 'EL', EL, '~', &
                                      XEL( NFLAGS*(I-1) + J ), &
                                      VTIGHT, OK )
               END IF

            END DO

         END DO

      END DO


!
!     *****************************************************************
!
!     Normal cases: AZLREC
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Loop through the canned cases. These cover the nominal
!     use of the routine.
!
      DO I = 1, NVECT

         DO J = 1, NFLAGS

            DO K = 1, NSCALE
!
!     --- Case: -------------------------------------------------------
!
               TITLE = 'Canned case # (AZLREC); AZCCW = #; ELPLSZ = #, ' &
               //      'SCALE = #.'
               CALL REPMI ( TITLE, '#', (NFLAGS * (I-1) +J), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', AZCCW(J),  'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', ELPLSZ(J), 'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', SCALES(K), 6, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE )

               R = SCALES(K) * XR( NFLAGS * (I-1) + J )

               CALL AZLREC ( R, &
                             RPD() * XAZ   ( NFLAGS * (I-1) + J), &
                             RPD() * XEL   ( NFLAGS * (I-1) + J), &
                                     AZCCW ( J                 ), &
                                     ELPLSZ( J                 ), &
                                     REC                         )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Test the vector relative error if the test point is not
!              at the origin.
!
               CALL VSCL ( SCALES(K), RECTAN(1,I), XREC )

               IF (  .NOT.  VZERO( RECTAN(1,I) )  ) THEN

                  CALL CHCKAD ( 'REC',  REC, '~~/', XREC, 3, &
                                VTIGHT, OK                  )
               ELSE
!
!                 Check component-wise differences.
!
                  CALL CHCKAD ( 'REC',  REC, '~', XREC, 3, VTIGHT, OK )

               END IF

            END DO

         END DO

      END DO


!
!     --- Case: -------------------------------------------------------
!
!     Confirm that AZLREC and RECAZL are inverses. Loop through
!     random cases. These rely on RECAZL.
!
      SEED = -100001

      DO I = 1, NRAND
!
!        Pick R so that its logarithm is uniformly distributed.
!
         R = EXP(  T_RANDD( LOG(1.D-300), LOG(1.D300), SEED )  )

         AZ = T_RANDD( 0.D0, TWOPI(), SEED )

         EL = T_RANDD( -HALFPI(), HALFPI(), SEED )

         DO J = 1, NFLAGS
!
!     --- Case: -------------------------------------------------------
!
            TITLE = 'Random case # (AZLREC and RECAZL); ' &
            //      'AZCCW = #; ELPLSZ = #. rand = #.'
            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPML ( TITLE, '#', AZCCW(J),  'C', TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPML ( TITLE, '#', ELPLSZ(J), 'C', TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

            CALL AZLREC ( R,  AZ, EL, AZCCW(J), ELPLSZ(J), REC )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Use RECAZL, which we presume to be valid, to invert the
!           conversion performed by AZLREC.
!
            CALL RECAZL ( REC, AZCCW(J), ELPLSZ(J), R2, AZ2, EL2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Use slightly loose tolerance for random inputs.
!
            CALL CHCKSD ( 'R2',  R2,  '~/', R,  TIGHT, OK )
            CALL CHCKSD ( 'AZ2', AZ2, '~/', AZ, TIGHT, OK )
            CALL CHCKSD ( 'EL2', EL2, '~/', EL, TIGHT, OK )

         END DO

      END DO


!
!     *****************************************************************
!
!     Normal cases: DRDAZL
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Loop through the canned cases. These cover the nominal
!     use of the routine.
!
      DO I = 1, NVECT

         DO J = 1, NFLAGS

            DO K = 1, NSCALE
!
!     --- Case: -------------------------------------------------------
!
               TITLE = 'Canned case # (DRDAZL); AZCCW = #; ELPLSZ = #, ' &
               //      'SCALE = #.'
               CALL REPMI ( TITLE, '#', (NFLAGS * (I-1) +J), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', AZCCW(J),  'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', ELPLSZ(J), 'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', SCALES(K), 6, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

!
!              Scale the input range. This will cause scaling of the
!              of the partial derivatives with respect to AZ and EL
!              of the rectangular coordinates.
!
               R = SCALES(K) * XR( NFLAGS * (I-1) + J)

               CALL DRDAZL ( R, &
                             RPD() * XAZ   ( NFLAGS * (I-1) + J ), &
                             RPD() * XEL   ( NFLAGS * (I-1) + J ), &
                                     AZCCW ( J                  ), &
                                     ELPLSZ( J                  ), &
                                     JACOBI                       )
               CALL CHCKXC ( .FALSE., ' ', OK )

               JAC2(1,1) = DXDR ( NFLAGS * (I-1) + J )
               JAC2(1,2) = DXDAZ( NFLAGS * (I-1) + J ) * SCALES(K)
               JAC2(1,3) = DXDEL( NFLAGS * (I-1) + J ) * SCALES(K)

               JAC2(2,1) = DYDR ( NFLAGS * (I-1) + J )
               JAC2(2,2) = DYDAZ( NFLAGS * (I-1) + J ) * SCALES(K)
               JAC2(2,3) = DYDEL( NFLAGS * (I-1) + J ) * SCALES(K)

               JAC2(3,1) = DZDR ( NFLAGS * (I-1) + J )
               JAC2(3,2) = DZDAZ( NFLAGS * (I-1) + J ) * SCALES(K)
               JAC2(3,3) = DZDEL( NFLAGS * (I-1) + J ) * SCALES(K)
!
!              Scale the expected matrix to account for the scale of
!              the input vector.
!
               IF ( SCALES(K) .EQ. 1.D0 ) THEN

                  CALL CHCKAD ( 'JACOBI', JACOBI, '~', &
                                 JAC2, 9, VTIGHT, OK )
               ELSE
                  CALL CHCKAD ( 'JACOBI', JACOBI, '~~/', &
                                 JAC2, 9, VTIGHT, OK )
               END IF

            END DO

         END DO

      END DO


!
!     *****************************************************************
!
!     Normal cases: DAZLDR
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Loop through the canned cases. These cover the nominal
!     use of the routine.
!
!     For testing DAZLDR, use only the first 4 vectors, as the
!     Jacobian matrix is undefined for points on the Z axis.
!
      DO I = 1, 4

         DO J = 1, NFLAGS

            DO K = 1, NSCALE
!
!     --- Case: -------------------------------------------------------
!
               TITLE = 'Canned case # (DAZLDR); AZCCW = #; ELPLSZ = #, ' &
               //      'SCALE = #.'

               CALL REPMI ( TITLE, '#', (NFLAGS * (I-1) +J), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', AZCCW(J),  'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', ELPLSZ(J), 'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', SCALES(K), 6, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

!
!              Scale the input vector. This will cause inverse
!              scaling of the partial derivatives of AZ and EL
!              with respect to the rectangular coordinates.
!
               CALL VSCL ( SCALES(K), RECTAN(1,I), REC )

               CALL DAZLDR ( REC(1),     REC(2),      REC(3), &
                             AZCCW( J ), ELPLSZ( J ), JACOBI )
               CALL CHCKXC ( .FALSE., ' ', OK )

               JAC2(1,1) = DXDR ( NFLAGS * (I-1) + J )
               JAC2(1,2) = DXDAZ( NFLAGS * (I-1) + J )
               JAC2(1,3) = DXDEL( NFLAGS * (I-1) + J )

               JAC2(2,1) = DYDR ( NFLAGS * (I-1) + J )
               JAC2(2,2) = DYDAZ( NFLAGS * (I-1) + J )
               JAC2(2,3) = DYDEL( NFLAGS * (I-1) + J )

               JAC2(3,1) = DZDR ( NFLAGS * (I-1) + J )
               JAC2(3,2) = DZDAZ( NFLAGS * (I-1) + J )
               JAC2(3,3) = DZDEL( NFLAGS * (I-1) + J )

               CALL INVORT ( JAC2, INJACO )

               DO M = 1, 3
!
!                 Row 2 of INJACO is the gradient of AZ.
!                 Row 3 of INJACO is the gradient of EL.
!
                  INJACO(2,M) = INJACO(2,M) / SCALES(K)
                  INJACO(3,M) = INJACO(3,M) / SCALES(K)

               END DO

               IF ( SCALES(K) .EQ. 1.D0 ) THEN

                  CALL CHCKAD ( 'JACOBI', JACOBI, '~', &
                                INJACO,   9,      VTIGHT, OK )
               ELSE

                  CALL CHCKAD ( 'JACOBI', JACOBI, '~~/', &
                                INJACO,   9,      VTIGHT, OK )
               END IF

            END DO

         END DO

      END DO


!
!     --- Case: -------------------------------------------------------
!
!     Confirm that DAZLDR and DRDAZL are inverses. Loop through
!     random cases.
!
      SEED = -100001

      CALL IDENT ( IDMAT )

      DO I = 1, NRAND
!
!        Pick R so that its logarithm is uniformly distributed.
!        Limit the scale of R to avoid cases where extreme roundoff
!        may occur.
!
         R = EXP(  T_RANDD( LOG(1.D-6), LOG(1.D9), SEED )  )

         AZ = T_RANDD( 0.D0, TWOPI(), SEED )

!
!        Bound the elevation away from the poles, where there is
!        great loss of precision.
!
         EL = T_RANDD( -HALFPI() + 1.D-3, HALFPI() - 1.D-3, SEED )

         DO J = 1, NFLAGS

!
!     --- Case: -------------------------------------------------------
!
            TITLE = 'Random case # (DAZLDR and DRDAZL); ' &
            //      'AZCCW = #; ELPLSZ = #.'
            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPML ( TITLE, '#', AZCCW(J),  'C', TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPML ( TITLE, '#', ELPLSZ(J), 'C', TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

            CALL DRDAZL ( R, AZ, EL, AZCCW(J), ELPLSZ(J), JACOBI )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL AZLREC ( R, AZ, EL, AZCCW(J), ELPLSZ(J), REC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DAZLDR ( REC(1),   REC(2),    REC(3), &
                          AZCCW(J), ELPLSZ(J), JAC2   )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL MXM ( JAC2, JACOBI, MPROD )

!
!           Use very loose tolerance---approximately single
!           precision---for the product of Jacobians produced from
!           random Range, AZ, EL coordinates. It appears this is the
!           best we can do, at least with the current Jacobian matrix
!           code.
!
            CALL CHCKAD ( 'MPROD', MPROD, '~~/', IDMAT, 9, LOOSE, OK )

         END DO

      END DO

!
!     *****************************************************************
!
!     Non-error exceptional cases: AZLREC
!
!     *****************************************************************
!

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'AZLREC: negative input range' )

      AZ = RPD() * 30
      EL = RPD() * 60
      R  = -1.0D0

      CALL AZLREC (  R, AZ, EL, .FALSE., .TRUE., REC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL AZLREC ( -R, AZ, EL, .FALSE., .TRUE., REC2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSCLIP ( -1.D0, REC2 )

      CALL CHCKAD( 'REC', REC, '~', REC2, 3, VTIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'AZLREC: AZ out of range' )

      AZ = RPD() * ( 30.D0 +  720.D0 )
      EL = RPD() * 60
      R  = 1.0D0

      CALL AZLREC (  R, AZ, EL, .FALSE., .TRUE., REC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      AZ = RPD() * 30
      CALL AZLREC (  R, AZ, EL, .FALSE., .TRUE., REC2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(+720) REC', REC, '~', REC2, 3, VTIGHT, OK )

      AZ = RPD() * ( 30.D0 -  720.D0 )

      CALL AZLREC (  R, AZ, EL,  .FALSE., .TRUE., REC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( '(-720) REC', REC, '~', REC2, 3, VTIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'AZLREC: EL out of range' )
!
!     Just make sure no error is signaled. The results are not
!     specified.
!
      AZ = RPD() * 30
      EL = ( RPD() * 90 ) + 1.D-14
      R  = 1.0D0

      CALL AZLREC (  R, AZ, EL, .FALSE., .TRUE., REC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      EL = -EL

      CALL AZLREC (  R, AZ, EL, .FALSE., .TRUE., REC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     *****************************************************************
!
!     Error cases: DRDAZL
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Error case: DRDAZL negative input range' )

      CALL DRDAZL ( -1.0D0, 0.0D0, 0.0D0, .TRUE., .TRUE., JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
!     *****************************************************************
!
!     Error cases: DAZLDR
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Test error handling for singularity. Use two cases, i.e.
!     vector #5 (0,0,1) and vector #7 (0,0,0), with different AZCCW
!     and ELPLSZ values, to verify independence with respect to the
!     flags and Z component of the vector.
!
      CALL TCASE ('Error case: Jacobian undefined for point on Z-axis')

      CALL DAZLDR ( RECTAN(1,5), RECTAN(2,5), RECTAN(3,5), &
                    .TRUE.,      .TRUE.,      JACOBI      )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTONZAXIS)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ('Error case: Jacobian undefined for point on origin.')

      CALL DAZLDR ( RECTAN(1,7), RECTAN(2,7), RECTAN(3,7), &
                    .FALSE.,     .TRUE.,      JACOBI      )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTONZAXIS)', OK )



      CALL T_SUCCESS ( OK )
      RETURN

      END
