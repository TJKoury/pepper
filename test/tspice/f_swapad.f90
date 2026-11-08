!$Procedure F_SWAPAD ( Test the SPICELIB routine SWAPAD )

      SUBROUTINE F_SWAPAD ( OK )

! $ Abstract
!
!     Exercise the SPICELIB sub-array swapping routine SWAPAD.
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
! $ Files
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
! $ Particulars
!
!     This routine tests the SPICELIB routine SWAPAD.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 14-OCT-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!


!
!     Local Parameters
!
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               LARGE
      PARAMETER           ( LARGE  = 1000 )

      INTEGER               NUMCAS
      PARAMETER           ( NUMCAS = 1000 )

      INTEGER               SMALL
      PARAMETER           ( SMALL  = 10 )

!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ARRAY0 ( LARGE )
      DOUBLE PRECISION      ARRAY1 ( LARGE )
      DOUBLE PRECISION      WORK   ( LARGE )
      DOUBLE PRECISION      XARRAY ( LARGE )

      INTEGER               I
      INTEGER               LOCM
      INTEGER               LOCN
      INTEGER               M
      INTEGER               N
      INTEGER               SIZE
      INTEGER               START

!
!     Saved values
!

!
!     Initial values
!



!
!     Open the test family.
!
      CALL TOPEN ( 'F_SWAPAD' )


!
!     We'll do an exhaustive set of tests on a small array.
!
      CALL CLEARI ( LARGE, ARRAY0 )

      SIZE = SMALL

      DO I = 1, SIZE
         ARRAY0(I) = I
      END DO

!
!     LOCN is the start of the "upper" slice.
!
      DO LOCN = 1, SIZE
!
!        N is the size of the upper slice.
!
         DO N = 0, SIZE-LOCN+1
!
!           LOCM is the start of the "lower" slice.
!
            START = LOCN + MAX ( 1, N )

            DO LOCM = START, SIZE
!
!              M is the size of the lower slice.
!
               DO M = 0, SIZE-LOCM+1
!
! --- Case: ------------------------------------------------------
!
                  TITLE = 'Case: N = #; LOCN = #; M = #; LOCM = #.'

                  CALL REPMI  ( TITLE, '#', N,    TITLE )
                  CALL REPMI  ( TITLE, '#', LOCN, TITLE )
                  CALL REPMI  ( TITLE, '#', M,    TITLE )
                  CALL REPMI  ( TITLE, '#', LOCM, TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE ( TITLE )

!
!                 Make two copies of the initial array.
!
                  CALL MOVED ( ARRAY0, SIZE, ARRAY1 )
                  CALL MOVED ( ARRAY0, SIZE, XARRAY )

!
!                 Swap the array slices indicated by LOCN, N, LOCM,
!                 and M.
!
                  CALL SWAPAD ( N, LOCN, M, LOCM, ARRAY1 )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Produce our expected array.
!
                  CALL T_SWAPAD ( SIZE, N,    LOCN, &
                                  M,    LOCM, XARRAY, WORK )

!
!                 Test our results.
!
                  CALL CHCKAD ( 'swapped array (0)', &
                                 ARRAY1,  '=', &
                                 XARRAY,  SIZE,  0.D0, OK  )


!
!                 Now we'll repeat the test with the order of
!                 the arguments swapped.
!
                  CALL MOVED ( ARRAY0, SIZE, ARRAY1 )
                  CALL MOVED ( ARRAY0, SIZE, XARRAY )

!
!                 Swap the array slices indicated by LOCN, N, LOCM,
!                 and M.
!
                  CALL SWAPAD ( M, LOCM, N, LOCN, ARRAY1 )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Produce our expected array.
!
                  CALL T_SWAPAD ( SIZE, M,    LOCM, &
                                  N,    LOCN, XARRAY, WORK )

!
!                 Test our results.
!
                  CALL CHCKAD ( 'swapped array (1)', &
                                 ARRAY1,  '=', &
                                 XARRAY,  SIZE,  0.D0, OK  )
               END DO

            END DO


         END DO

      END DO


!
!     Now for some error handling tests.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SWAPAD: overlapping array slices.' )

      CALL SWAPAD ( 2, 1, 2, 1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTDISTINCT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SWAPAD: non-positive value of LOCN, LOCM' )

      CALL SWAPAD (  2,  0,  2,  1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

      CALL SWAPAD (  2, -1,  2,  1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

      CALL SWAPAD (  2,  1,  2, -1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

      CALL SWAPAD (  2,  1,  2,  0, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDINDEX)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'SWAPAD: Negative value of N, M' )

      CALL SWAPAD ( -2,  0,  2,  1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARGUMENT)', OK )

      CALL SWAPAD (  2,  0, -2,  1, ARRAY1 )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARGUMENT)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


!
!     T_SWAPAD is a utility routine used for testing SWAPAD. T_SWAPAD
!     uses an alternate implementation of the swapping algorithm that
!     uses work space to build the output array:  the swapping
!     algorithm is quite simple when the constraint of in-place
!     operation is removed.
!
!     This routine does no error checking.
!
      SUBROUTINE T_SWAPAD ( SIZE, N, LOCN, M, LOCM, ARRAY, WORK )
      IMPLICIT NONE

      INTEGER               SIZE
      INTEGER               N
      INTEGER               LOCN
      INTEGER               M
      INTEGER               LOCM
      DOUBLE PRECISION      ARRAY ( * )
      DOUBLE PRECISION      WORK  ( * )

!
!     Local variables
!
      INTEGER               LOC
      INTEGER               LOWER
      INTEGER               NLOW
      INTEGER               NMOVE
      INTEGER               NUP
      INTEGER               TO
      INTEGER               UPPER


!
!     We'll build the output array in WORK; then we'll copy the
!     result back to ARRAY.
!
!     Identify the start indices of the "top" and "bottom" array slices
!     to be swapped.  We consider the lower addresses to be at the
!     "top" of the array.
!
      UPPER = MIN ( LOCN, LOCM )
      LOWER = MAX ( LOCN, LOCM )

      IF ( UPPER .EQ. LOCN ) THEN
         NUP  = N
         NLOW = M
      ELSE
         NUP  = M
         NLOW = N
      END IF

!
!     Move the elements preceding UPPER into WORK.
!
      TO    = 1
      NMOVE = UPPER - 1
      CALL MOVED ( ARRAY(1),      NMOVE,  WORK(TO) )

!
!     Move the elements in the lower slice into WORK.
!
      TO    = TO + NMOVE
      NMOVE = NLOW
      CALL MOVED ( ARRAY(LOWER),  NMOVE,  WORK(TO) )

!
!     Move the elements between the slices into WORK.
!
      TO     =  TO + NMOVE
      NMOVE  =  LOWER - 1 - ( UPPER + NUP - 1 )

      CALL MOVED ( ARRAY(UPPER+NUP),  NMOVE,  WORK(TO) )

!
!     Move the elements in the upper slice into WORK.
!
      TO     =  TO + NMOVE
      NMOVE  =  NUP
      CALL MOVED ( ARRAY(UPPER),  NMOVE,  WORK(TO) )

!
!     Move the elements below the lower slice into WORK.
!
      TO     =  TO + NMOVE
      LOC    =  LOWER + NLOW
      NMOVE  =  SIZE - ( LOC - 1 )

      CALL MOVED ( ARRAY(LOC),  NMOVE,  WORK(TO) )

!
!     Copy WORK into ARRAY.
!
      CALL MOVED ( WORK, SIZE, ARRAY )

      END



