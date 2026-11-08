C$Procedure F_AZL ( Azimuth/elevation coordinate tests )

      SUBROUTINE F_AZL ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB azimuth/elevation coordinate
C     conversion and Jacobian routines.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB azimuth/elevation coordinate
C     and coordinate derivative conversion routines
C
C        AZLREC
C        DAZLDR
C        DRDAZL
C        RECAZL
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 22-DEC-2020 (JDR) (NJB)
C
C-&


C$ Index_Entries
C
C     Test family for AZ/EL coordinate routines
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      T_RANDD
      DOUBLE PRECISION      TWOPI

      LOGICAL               VZERO

C
C     Local parameters
C
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

C
C     Local variables
C
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

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA                 E3 / 0.D0, 0.D0, 1.D0 /

      DATA                ( ( RECTAN(J,I), J = 1, 3 ), I = 1, NVECT )  /
     .                               1.000D0,     0.000D0,     0.000D0,
     .                              -1.000D0,     0.000D0,     0.000D0,
     .                               0.000D0,     1.000D0,     0.000D0,
     .                               0.000D0,    -1.000D0,     0.000D0,
     .                               0.000D0,     0.000D0,     1.000D0,
     .                               0.000D0,     0.000D0,    -1.000D0,
     .                               0.000D0,     0.000D0,     0.000D0
     .                              /

      DATA                 ( AZCCW(I), ELPLSZ(I), I = 1, 4 ) /
     .                         .TRUE.,    .TRUE.,
     .                         .TRUE.,   .FALSE.,
     .                        .FALSE.,    .TRUE.,
     .                        .FALSE.,   .FALSE.
     .                       /

      DATA       ( XR(I),     XAZ(I),     XEL(I), I = 1, 19 ) /
     .           1.000D0,    0.000D0,    0.000D0,
     .           1.000D0,    0.000D0,    0.000D0,
     .           1.000D0,    0.000D0,    0.000D0,
     .           1.000D0,    0.000D0,    0.000D0,
     .           1.000D0,  180.000D0,    0.000D0,
     .           1.000D0,  180.000D0,    0.000D0,
     .           1.000D0,  180.000D0,    0.000D0,
     .           1.000D0,  180.000D0,    0.000D0,
     .           1.000D0,   90.000D0,    0.000D0,
     .           1.000D0,   90.000D0,    0.000D0,
     .           1.000D0,  270.000D0,    0.000D0,
     .           1.000D0,  270.000D0,    0.000D0,
     .           1.000D0,  270.000D0,    0.000D0,
     .           1.000D0,  270.000D0,    0.000D0,
     .           1.000D0,   90.000D0,    0.000D0,
     .           1.000D0,   90.000D0,    0.000D0,
     .           1.000D0,    0.000D0,   90.000D0,
     .           1.000D0,    0.000D0,  -90.000D0,
     .           1.000D0,    0.000D0,   90.000D0 /

      DATA       ( XR(I),     XAZ(I),     XEL(I), I = 20, NCASE ) /
     .           1.000D0,    0.000D0,  -90.000D0,
     .           1.000D0,    0.000D0,  -90.000D0,
     .           1.000D0,    0.000D0,   90.000D0,
     .           1.000D0,    0.000D0,  -90.000D0,
     .           1.000D0,    0.000D0,   90.000D0,
     .           0.000D0,    0.000D0,    0.000D0,
     .           0.000D0,    0.000D0,    0.000D0,
     .           0.000D0,    0.000D0,    0.000D0,
     .           0.000D0,    0.000D0,    0.000D0 /

C
C     The equations for the transformation between rectangular and
C     azimuth/elevation coordinates, taking into account the possible
C     ways of measuring azimuth and elevation are as follows:
C
C        X = R * DCOS( AZSNSE * AZ ) * DCOS( ELDIR * EL )
C        Y = R * DSIN( AZSNSE * AZ ) * DCOS( ELDIR * EL )
C        Z = R *                       DSIN( ELDIR * EL )
C
C     or equivalently, since cosine is an even function,
C
C        X = R * DCOS( AZ )          * DCOS( EL )
C        Y = R * DSIN( AZSNSE * AZ ) * DCOS( EL )
C        Z = R * DSIN( ELDIR  * EL )
C
C     The following data block contains test values for
C
C        DX/DR  =               DCOS( AZSNSE * AZ ) * DCOS( ELDIR * EL )
C        DX/DAZ = -R * AZSNSE * DSIN( AZSNSE * AZ ) * DCOS( ELDIR * EL )
C        DX/DEL = -R * ELDIR  * DCOS( AZSNSE * AZ ) * DSIN( ELDIR * EL )
C
      DATA      ( DXDR(I),   DXDAZ(I),  DXDEL(I), I = 1, 19 ) /
     .            1.000D0,    0.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .            0.000D0,   -1.000D0,   0.000D0,
     .            0.000D0,   -1.000D0,   0.000D0,
     .            0.000D0,    1.000D0,   0.000D0,
     .            0.000D0,    1.000D0,   0.000D0,
     .            0.000D0,    1.000D0,   0.000D0,
     .            0.000D0,    1.000D0,   0.000D0,
     .            0.000D0,   -1.000D0,   0.000D0,
     .            0.000D0,   -1.000D0,   0.000D0,
     .            0.000D0,    0.000D0,  -1.000D0,
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,  -1.000D0  /

      DATA      ( DXDR(I),   DXDAZ(I),  DXDEL(I), I = 20, NCASE ) /
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,  -1.000D0,
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,  -1.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0 /

C
C     The following data block contains test values for
C
C        DY/DR  =               DSIN( AZSNSE * AZ ) * DCOS( ELDIR * EL )
C        DY/DAZ =  R * AZSNSE * DCOS( AZSNSE * AZ ) * DCOS( ELDIR * EL )
C        DY/DEL = -R * ELDIR  * DSIN( AZSNSE * AZ ) * DSIN( ELDIR * EL )
C
C     or equivalently, since cosine is an even function,
C
C        DY/DR  =  DSIN( AZSNSE * AZ ) * DCOS( EL )
C        DY/DAZ =  R * AZSNSE * DCOS( AZ ) * DCOS( EL )
C        DY/DEL = -R * ELDIR  * DSIN( AZSNSE * AZ ) * DSIN( ELDIR * EL )

      DATA      ( DYDR(I),   DYDAZ(I),  DYDEL(I), I = 1, 19 ) /
     .            0.000D0,    1.000D0,   0.000D0,
     .            0.000D0,    1.000D0,   0.000D0,
     .            0.000D0,   -1.000D0,   0.000D0,
     .            0.000D0,   -1.000D0,   0.000D0,
     .            0.000D0,   -1.000D0,   0.000D0,
     .            0.000D0,   -1.000D0,   0.000D0,
     .            0.000D0,    1.000D0,   0.000D0,
     .            0.000D0,    1.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0  /

      DATA      ( DYDR(I),   DYDAZ(I),  DYDEL(I), I = 20, NCASE ) /
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0  /

C
C     The following data block contains test values for
C
C        DZ/DR  =              DSIN( ELDIR * EL )
C        DZ/DAZ =              0.000D0
C        DZ/DEL = R * ELDIR  * DCOS( ELDIR * EL )
C
      DATA      ( DZDR(I),   DZDAZ(I),  DZDEL(I), I = 1, 19 ) /
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,  -1.000D0,
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,  -1.000D0,
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,  -1.000D0,
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,  -1.000D0,
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,  -1.000D0,
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,  -1.000D0,
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,  -1.000D0,
     .            0.000D0,    0.000D0,   1.000D0,
     .            0.000D0,    0.000D0,  -1.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0,
     .            1.000D0,    0.000D0,   0.000D0 /

      DATA      ( DZDR(I),   DZDAZ(I),  DZDEL(I), I = 20, NCASE ) /
     .            1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .           -1.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0,
     .            0.000D0,    0.000D0,   0.000D0
     .            /

C
C     Scales used for range values:
C
      DATA SCALES / 1.0D-300, 0.5D0, 1.D0, 2.D0, 1.0D300 /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_AZL' )


C
C     *****************************************************************
C
C     Normal cases: RECAZL
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Loop through the canned cases. These cover the nominal
C     use of the routine.
C
      DO I = 1, NVECT

         DO J = 1, NFLAGS

            DO K = 1, NSCALE
C
C     --- Case: -------------------------------------------------------
C
               TITLE = 'Canned case # (RECAZL); AZCCW = #; ELPLSZ = #, '
     .         //      'SCALE = #.'
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

               CALL CHCKSD ( 'AZ',    AZ, '~', XAZ( NFLAGS*(I-1) + J ),
     .                       VTIGHT, OK )

               CALL CHCKSD ( 'EL',    EL, '~', XEL( NFLAGS*(I-1) + J ),
     .                       VTIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C

C
C              Update the title from the previous case.
C
               CALL REPMC ( TITLE, 'Canned', 'Perturbed AZ', TITLE )

               CALL TCASE( TITLE )

               CALL VSCL ( SCALES(K), RECTAN(1,I), REC )

C
C              Perturb the input vector as long as we don't have a
C              degenerate case.
C
               IF (      ( REC(1) .NE. 0.D0 )
     .              .OR. ( REC(2) .NE. 0.D0 )  ) THEN
C
C                 Rotate the input vector by +30 degrees in the
C                 positive AZ direction.
C
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

C
C              Subtract the delta from the returned azimuth for
C              comparison against the canned expected result.
C
               AZ = AZ - DELTAZ

C
C              Convert AZ and EL to degrees for comparison with expected
C              values.
C
               AZ = DPR() * AZ

               EL = DPR() * EL

C
C              Move AZ into the range [ XAZ - 360, XAZ + 360 ]
C              if necessary. We presume AZ and XAZ differ by a
C              very small amount.
C
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
C
C              Note slightly relaxed tolerance for this case.
C
               CALL CHCKSD ( 'AZ',    AZ, '~', XAZ( NFLAGS*(I-1) + J ),
     .                       TIGHT, OK )

               CALL CHCKSD ( 'EL',    EL, '~', XEL( NFLAGS*(I-1) + J ),
     .                       VTIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C

C
C              Update the title from the previous case.
C
               CALL REPMC ( TITLE, 'Perturbed AZ',
     .                             'Perturbed AZ and EL', TITLE )

               CALL TCASE( TITLE )

               CALL VSCL ( SCALES(K), RECTAN(1,I), REC )

C
C              Perturb AZ of the input vector as long as we don't have
C              a degenerate case.
C
               IF (      ( REC(1) .NE. 0.D0 )
     .              .OR. ( REC(2) .NE. 0.D0 )  ) THEN
C
C                 Rotate the input vector by +30 degrees in the
C                 positive AZ direction.
C
                  DELTAZ = RPD() * 30
               ELSE
                  DELTAZ = 0.D0
               END IF

               IF ( AZCCW(J) ) THEN
                  CALL VROTV ( REC, E3,  DELTAZ, REC2 )
               ELSE
                  CALL VROTV ( REC, E3, -DELTAZ, REC2 )
               END IF

C
C              Set EL of the input vector if the original EL is
C              0 degrees but the input is not the zero vector.
C
               IF (       (  RECTAN(3,I) .EQ. 0.D0       )
     .              .AND. (  .NOT. VZERO( RECTAN(1,I) )  ) ) THEN
C
C                 Set the Z component of REC2 to be consistent with
C                 an elevation magnitude of 60 degrees.
C
                  IF ( ELPLSZ(J) ) THEN
                     REC2(3) =  ( SQRT(3.D0)/2 ) * SCALES(K)
                  ELSE
                     REC2(3) = -( SQRT(3.D0)/2 ) * SCALES(K)
                  END IF
C
C                 Scale the X and Y components of REC2 to be consistent
C                 with the elevation.
C
                  REC2(1) = REC2(1) / 2
                  REC2(2) = REC2(2) / 2

               END IF


               CALL RECAZL ( REC2, AZCCW(J), ELPLSZ(J), R, AZ, EL )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Subtract the delta from the returned azimuth for
C              comparison against the canned expected result.
C
               AZ = AZ - DELTAZ

C
C              Convert AZ and EL to degrees for comparison with expected
C              values.
C
               AZ = DPR() * AZ
               EL = DPR() * EL

C
C              Move AZ into the range [ XAZ - 360, XAZ + 360 ]
C              if necessary. We presume AZ and XAZ differ by a
C              very small amount.
C
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
C
C              Note slightly relaxed tolerance for this case.
C
               CALL CHCKSD ( 'AZ',    AZ, '~', XAZ( NFLAGS*(I-1) + J ),
     .                       TIGHT, OK )

               IF (       (  RECTAN(3,I) .EQ. 0.D0       )
     .              .AND. (  .NOT. VZERO( RECTAN(1,I) )  ) ) THEN

                  XEL2 = 60.D0

                  CALL CHCKSD ( 'EL', EL, '~', XEL2, VTIGHT, OK )

               ELSE

                  CALL CHCKSD ( 'EL', EL, '~',
     .                                XEL( NFLAGS*(I-1) + J ),
     .                                VTIGHT, OK )
               END IF

            END DO

         END DO

      END DO


C
C     *****************************************************************
C
C     Normal cases: AZLREC
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Loop through the canned cases. These cover the nominal
C     use of the routine.
C
      DO I = 1, NVECT

         DO J = 1, NFLAGS

            DO K = 1, NSCALE
C
C     --- Case: -------------------------------------------------------
C
               TITLE = 'Canned case # (AZLREC); AZCCW = #; ELPLSZ = #, '
     .         //      'SCALE = #.'
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

               CALL AZLREC ( R,
     .                       RPD() * XAZ   ( NFLAGS * (I-1) + J),
     .                       RPD() * XEL   ( NFLAGS * (I-1) + J),
     .                               AZCCW ( J                 ),
     .                               ELPLSZ( J                 ),
     .                               REC                         )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Test the vector relative error if the test point is not
C              at the origin.
C
               CALL VSCL ( SCALES(K), RECTAN(1,I), XREC )

               IF (  .NOT.  VZERO( RECTAN(1,I) )  ) THEN

                  CALL CHCKAD ( 'REC',  REC, '~~/', XREC, 3,
     .                          VTIGHT, OK                  )
               ELSE
C
C                 Check component-wise differences.
C
                  CALL CHCKAD ( 'REC',  REC, '~', XREC, 3, VTIGHT, OK )

               END IF

            END DO

         END DO

      END DO


C
C     --- Case: -------------------------------------------------------
C
C     Confirm that AZLREC and RECAZL are inverses. Loop through
C     random cases. These rely on RECAZL.
C
      SEED = -100001

      DO I = 1, NRAND
C
C        Pick R so that its logarithm is uniformly distributed.
C
         R = EXP(  T_RANDD( LOG(1.D-300), LOG(1.D300), SEED )  )

         AZ = T_RANDD( 0.D0, TWOPI(), SEED )

         EL = T_RANDD( -HALFPI(), HALFPI(), SEED )

         DO J = 1, NFLAGS
C
C     --- Case: -------------------------------------------------------
C
            TITLE = 'Random case # (AZLREC and RECAZL); '
     .      //      'AZCCW = #; ELPLSZ = #. rand = #.'
            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPML ( TITLE, '#', AZCCW(J),  'C', TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL REPML ( TITLE, '#', ELPLSZ(J), 'C', TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

            CALL AZLREC ( R,  AZ, EL, AZCCW(J), ELPLSZ(J), REC )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Use RECAZL, which we presume to be valid, to invert the
C           conversion performed by AZLREC.
C
            CALL RECAZL ( REC, AZCCW(J), ELPLSZ(J), R2, AZ2, EL2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Use slightly loose tolerance for random inputs.
C
            CALL CHCKSD ( 'R2',  R2,  '~/', R,  TIGHT, OK )
            CALL CHCKSD ( 'AZ2', AZ2, '~/', AZ, TIGHT, OK )
            CALL CHCKSD ( 'EL2', EL2, '~/', EL, TIGHT, OK )

         END DO

      END DO


C
C     *****************************************************************
C
C     Normal cases: DRDAZL
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Loop through the canned cases. These cover the nominal
C     use of the routine.
C
      DO I = 1, NVECT

         DO J = 1, NFLAGS

            DO K = 1, NSCALE
C
C     --- Case: -------------------------------------------------------
C
               TITLE = 'Canned case # (DRDAZL); AZCCW = #; ELPLSZ = #, '
     .         //      'SCALE = #.'
               CALL REPMI ( TITLE, '#', (NFLAGS * (I-1) +J), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', AZCCW(J),  'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', ELPLSZ(J), 'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', SCALES(K), 6, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

C
C              Scale the input range. This will cause scaling of the
C              of the partial derivatives with respect to AZ and EL
C              of the rectangular coordinates.
C
               R = SCALES(K) * XR( NFLAGS * (I-1) + J)

               CALL DRDAZL ( R,
     .                       RPD() * XAZ   ( NFLAGS * (I-1) + J ),
     .                       RPD() * XEL   ( NFLAGS * (I-1) + J ),
     .                               AZCCW ( J                  ),
     .                               ELPLSZ( J                  ),
     .                               JACOBI                       )
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
C
C              Scale the expected matrix to account for the scale of
C              the input vector.
C
               IF ( SCALES(K) .EQ. 1.D0 ) THEN

                  CALL CHCKAD ( 'JACOBI', JACOBI, '~',
     .                           JAC2, 9, VTIGHT, OK )
               ELSE
                  CALL CHCKAD ( 'JACOBI', JACOBI, '~~/',
     .                           JAC2, 9, VTIGHT, OK )
               END IF

            END DO

         END DO

      END DO


C
C     *****************************************************************
C
C     Normal cases: DAZLDR
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Loop through the canned cases. These cover the nominal
C     use of the routine.
C
C     For testing DAZLDR, use only the first 4 vectors, as the
C     Jacobian matrix is undefined for points on the Z axis.
C
      DO I = 1, 4

         DO J = 1, NFLAGS

            DO K = 1, NSCALE
C
C     --- Case: -------------------------------------------------------
C
               TITLE = 'Canned case # (DAZLDR); AZCCW = #; ELPLSZ = #, '
     .         //      'SCALE = #.'

               CALL REPMI ( TITLE, '#', (NFLAGS * (I-1) +J), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', AZCCW(J),  'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPML ( TITLE, '#', ELPLSZ(J), 'C', TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', SCALES(K), 6, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

C
C              Scale the input vector. This will cause inverse
C              scaling of the partial derivatives of AZ and EL
C              with respect to the rectangular coordinates.
C
               CALL VSCL ( SCALES(K), RECTAN(1,I), REC )

               CALL DAZLDR ( REC(1),     REC(2),      REC(3),
     .                       AZCCW( J ), ELPLSZ( J ), JACOBI )
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
C
C                 Row 2 of INJACO is the gradient of AZ.
C                 Row 3 of INJACO is the gradient of EL.
C
                  INJACO(2,M) = INJACO(2,M) / SCALES(K)
                  INJACO(3,M) = INJACO(3,M) / SCALES(K)

               END DO

               IF ( SCALES(K) .EQ. 1.D0 ) THEN

                  CALL CHCKAD ( 'JACOBI', JACOBI, '~',
     .                          INJACO,   9,      VTIGHT, OK )
               ELSE

                  CALL CHCKAD ( 'JACOBI', JACOBI, '~~/',
     .                          INJACO,   9,      VTIGHT, OK )
               END IF

            END DO

         END DO

      END DO


C
C     --- Case: -------------------------------------------------------
C
C     Confirm that DAZLDR and DRDAZL are inverses. Loop through
C     random cases.
C
      SEED = -100001

      CALL IDENT ( IDMAT )

      DO I = 1, NRAND
C
C        Pick R so that its logarithm is uniformly distributed.
C        Limit the scale of R to avoid cases where extreme roundoff
C        may occur.
C
         R = EXP(  T_RANDD( LOG(1.D-6), LOG(1.D9), SEED )  )

         AZ = T_RANDD( 0.D0, TWOPI(), SEED )

C
C        Bound the elevation away from the poles, where there is
C        great loss of precision.
C
         EL = T_RANDD( -HALFPI() + 1.D-3, HALFPI() - 1.D-3, SEED )

         DO J = 1, NFLAGS

C
C     --- Case: -------------------------------------------------------
C
            TITLE = 'Random case # (DAZLDR and DRDAZL); '
     .      //      'AZCCW = #; ELPLSZ = #.'
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

            CALL DAZLDR ( REC(1),   REC(2),    REC(3),
     .                    AZCCW(J), ELPLSZ(J), JAC2   )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL MXM ( JAC2, JACOBI, MPROD )

C
C           Use very loose tolerance---approximately single
C           precision---for the product of Jacobians produced from
C           random Range, AZ, EL coordinates. It appears this is the
C           best we can do, at least with the current Jacobian matrix
C           code.
C
            CALL CHCKAD ( 'MPROD', MPROD, '~~/', IDMAT, 9, LOOSE, OK )

         END DO

      END DO

C
C     *****************************************************************
C
C     Non-error exceptional cases: AZLREC
C
C     *****************************************************************
C

C
C     --- Case: -------------------------------------------------------
C
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

C
C     --- Case: -------------------------------------------------------
C
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

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'AZLREC: EL out of range' )
C
C     Just make sure no error is signaled. The results are not
C     specified.
C
      AZ = RPD() * 30
      EL = ( RPD() * 90 ) + 1.D-14
      R  = 1.0D0

      CALL AZLREC (  R, AZ, EL, .FALSE., .TRUE., REC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      EL = -EL

      CALL AZLREC (  R, AZ, EL, .FALSE., .TRUE., REC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     *****************************************************************
C
C     Error cases: DRDAZL
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Error case: DRDAZL negative input range' )

      CALL DRDAZL ( -1.0D0, 0.0D0, 0.0D0, .TRUE., .TRUE., JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C     *****************************************************************
C
C     Error cases: DAZLDR
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Test error handling for singularity. Use two cases, i.e.
C     vector #5 (0,0,1) and vector #7 (0,0,0), with different AZCCW
C     and ELPLSZ values, to verify independence with respect to the
C     flags and Z component of the vector.
C
      CALL TCASE ('Error case: Jacobian undefined for point on Z-axis')

      CALL DAZLDR ( RECTAN(1,5), RECTAN(2,5), RECTAN(3,5),
     .              .TRUE.,      .TRUE.,      JACOBI      )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTONZAXIS)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ('Error case: Jacobian undefined for point on origin.')

      CALL DAZLDR ( RECTAN(1,7), RECTAN(2,7), RECTAN(3,7),
     .              .FALSE.,     .TRUE.,      JACOBI      )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTONZAXIS)', OK )



      CALL T_SUCCESS ( OK )
      RETURN

      END
