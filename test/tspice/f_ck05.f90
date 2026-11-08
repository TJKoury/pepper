!$Procedure F_CK05 ( CK data type 05 tests )

      SUBROUTINE F_CK05 ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB CK type 5 routines.
!     This is rudimentary set of tests to make sure the routines
!     cycle in every environment; thorough tests are performed
!     in the tspice_c test package.
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

      INCLUDE 'ck05.inc'

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
!     This routine tests routines that write and read type 05 CK
!     data.
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
!-    SPICELIB Version 1.1.0, 27-JAN-2014 (NJB)
!
!        Added header.
!
!        Removed unused declaration of parameter MAXDEG.
!
!-    SPICELIB Version 1.0.0, 06-SEP-2002 (NJB)
!
!-&



!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         CK05
      PARAMETER           ( CK05   = 'ck05_test.bc' )

      CHARACTER*(*)         REF
      PARAMETER           ( REF    = 'J2000' )


      INTEGER               BIGN
      PARAMETER           ( BIGN   = 200 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 40 )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-12 )


!
!     Local Variables
!
      CHARACTER*(SIDLEN)    SEGID

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      CLKOUT
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      EPOCHS ( 0 : BIGN-1 )
      DOUBLE PRECISION      Q      ( 0 : 3 )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      T1PACK ( C05PS1, 0 : BIGN-1 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               DEGREE
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               INST

      LOGICAL               AVFLAG
      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE                  Z

!
!     Initial values
!
      DATA                  Z / 0.D0, 0.D0, 1.D0 /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_CK05' )

!
!     Create and load  leapseconds kernel.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Define some CK data sets for use in testing.  First set:  a
!     sequence of rotations about the z-axis.  The C-matrix starts
!     out as the identity.  The ith attitude is obtained by
!     rotating about the z-axis by i microradians relative to the
!     (i-1)st attitude.
!
!     The ith epoch is simply i.
!

      ANGLE = 0.D0
      SCALE = 1.D-9

      DO I = 0, BIGN-1

         ANGLE = ANGLE - I * SCALE

         CALL AXISAR ( Z, ANGLE, CMAT )

         CALL M2Q ( CMAT, T1PACK(1,I) )

         EPOCHS(I) =  DBLE(I)

      END DO

      CALL TCASE (  'CKW05 test:  create a new CK containing '// &
                    'segment of subtype 1.'                    )


      AVFLAG = .TRUE.
      RATE   = 1000.D0
      SEGID  = 'CK type 05 test segment.'
      INST   = 1
      DEGREE = 11

!
!     Open a new CK file.
!
      IF ( EXISTS(CK05) ) THEN
         CALL DELFIL ( CK05 )
      END IF

      CALL CKOPN ( CK05, ' ', 0, HANDLE )

      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CKW05 ( HANDLE, C05TP1, DEGREE, EPOCHS(0), EPOCHS(BIGN-1), &
                   INST,   REF,    AVFLAG, SEGID,     BIGN, &
                   EPOCHS, T1PACK, RATE,   1,         EPOCHS        )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CKCLS ( HANDLE )



      CALL TCASE ( 'Recover pointing from segment of subtype 1 in '// &
                   'CK file CK05'                                  )

!
!     Now we'll use the CK user-level readers to look up pointing.
!
      CALL FURNSH ( CK05 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = 0.D0

      DO I = 0, BIGN - 1

         CALL CKGPAV ( INST,   EPOCHS(I), TOL, REF, CMAT, AV, &
                       CLKOUT, FOUND                          )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         IF ( FOUND ) THEN

            CALL CHCKSD ( 'CLKOUT',   CLKOUT, '~', &
                           EPOCHS(I), TIGHT,   OK )

            CALL M2Q ( CMAT, Q )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( 'Q', Q, '~', T1PACK(1,I), 4, TIGHT, OK )

         END IF


      END DO

      CALL UNLOAD ( CK05 )

      IF ( EXISTS(CK05) ) THEN
         CALL DELFIL ( CK05 )
      END IF


      CALL T_SUCCESS ( OK )
      RETURN
      END
