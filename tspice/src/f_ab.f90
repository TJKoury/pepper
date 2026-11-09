!$Procedure      F_AB ( Test DSK AB list routines )

      SUBROUTINE F_AB ( OK )
      use pepper_inc_dsk02

! $ Abstract
!
!     Test the AB list routines used by the DSK subsystem.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     TEST
!     UTILITY
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
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the private AB list routines
!
!        ZZADDLNK
!        ZZINILNK
!        ZZTRVLNK
!        ZZUNTNGL
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     This routine currently does not exercise DSKX02.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!     W.L. Taber       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 06-JAN-2016  (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB functions
!

!
!     Local parameters
!
      INTEGER               MXCELL
      PARAMETER           ( MXCELL = 300 )

      INTEGER               MAXA
      PARAMETER           ( MAXA = 100 )

      INTEGER               MAXB
      PARAMETER           ( MAXB = 200 )

      INTEGER               NULPTR
      PARAMETER           ( NULPTR = -1 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NAVALS
      PARAMETER           ( NAVALS = 3 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    LABEL

      INTEGER               ALIST  ( MAXA )
      INTEGER               AVALS  ( NAVALS )
      INTEGER               BLIST  ( MAXB )
      INTEGER               BVAL
      INTEGER               CELLS  ( 2, MXCELL )
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               NB
      INTEGER               NCELL
      INTEGER               XVAL
      INTEGER               XVALS  ( MXCELL )


!
!     Saved variables
!
      SAVE

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_AB' )


!***********************************************************************
!
!     ZZINILNK tests
!
!***********************************************************************

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad cell count' )

      CALL ZZINILNK ( MAXA, 0, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad pointer count' )

      CALL ZZINILNK ( 0, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Initialize an AB list' )


      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The A list should have all entries initialized to "null."
!
      CALL FILLI ( NULPTR, MAXA, XVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAI ( 'ALIST', ALIST, '=', XVALS, MAXA, OK )

!
!     The cell array should have all value elements set to 0.
!
      DO I = 1, MXCELL

         LABEL = 'CELLS(1,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, CELLS(1,I), '=', 0, 0, OK )

      END DO

!
!     The cell array should have all pointer elements set to NULPTR.
!
      DO I = 1, MXCELL

         LABEL = 'CELLS(2,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, CELLS(2,I), '=', NULPTR, 0, OK )

      END DO

!
!     The pointer array should have all pointer elements set to NULPTR.
!
      DO I = 1, MAXA

         LABEL = 'ALIST(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, ALIST(I), '=', NULPTR, 0, OK )

      END DO

!
!     The used cell count should be zero on output.
!
      CALL CHCKSI ( 'NCELL', NCELL, '=', 0, 0, OK )


!***********************************************************************
!
!     ZZADDLNK tests
!
!***********************************************************************

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'AVAL out of range' )

      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZADDLNK ( 0, 1, MAXA, MXCELL, ALIST, NCELL, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(AVALOUTOFRANGE)', OK )


      CALL ZZADDLNK ( -1, 1, MAXA, MXCELL, ALIST, NCELL, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(AVALOUTOFRANGE)', OK )


      CALL ZZADDLNK ( MAXA+1, 1, MAXA, MXCELL, ALIST, NCELL, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(AVALOUTOFRANGE)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Too many cell entries.' )


      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, MXCELL

         CALL ZZADDLNK ( 1, I, MAXA, MXCELL, ALIST, NCELL, CELLS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL ZZADDLNK ( 1, I+1, MAXA, MXCELL, ALIST, NCELL, CELLS )
      CALL CHCKXC ( .TRUE., 'SPICE(CELLARRAYTOOSMALL)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Build trivial list for one A-value.' )


      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, MXCELL

         CALL ZZADDLNK ( 1, I, MAXA, MXCELL, ALIST, NCELL, CELLS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check NCELL.
!
      CALL CHCKSI ( 'NCELL', NCELL, '=', MXCELL, 0, OK )

!
!     Check the pointer array. The first pointer should point to
!     the last cell; the other pointers should be null.
!
      CALL CHCKSI ( 'ALIST(1)', ALIST(1), '=', MXCELL, 0, OK )

      DO I = 2, MAXA

         LABEL = 'ALIST(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, ALIST(I), '=', -1, 0, OK )

      END DO

!
!     Check the cell entries.
!
      DO I = MXCELL, 1, -1

         LABEL = 'CELLS(1,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, CELLS(1,I), '=', I, 0, OK )


         LABEL = 'CELLS(2,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .GT. 1 ) THEN

            XVAL = I-1
         ELSE
            XVAL =  -1
         END IF

         CALL CHCKSI ( LABEL, CELLS(2,I), '=', XVAL, 0, OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Build a singleton list for each A-value.' )


      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = MAXA, 1, -1

         CALL ZZADDLNK ( I, -I, MAXA, MXCELL, ALIST, NCELL, CELLS )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Check NCELL.
!
      CALL CHCKSI ( 'NCELL', NCELL, '=', MAXA, 0, OK )

!
!     Check the pointer array. The Ith pointer should point to
!     the cell at index MAXA+1-I.
!
      DO I = 1, MAXA

         LABEL = 'ALIST(@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, ALIST(I), '=', MAXA+1-I, 0, OK )

      END DO

!
!     Check the cell entries.
!
      DO I = 1, MAXA

         LABEL = 'CELLS(1,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XVAL = -( MAXA+1-I )

         CALL CHCKSI ( LABEL, CELLS(1,I), '=', XVAL, 0, OK )


         LABEL = 'CELLS(2,@)'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XVAL =  -1

         CALL CHCKSI ( LABEL, CELLS(2,I), '=', XVAL, 0, OK )

      END DO



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Build a list for each of three A-values, where ' &
      //           'the list additions are interleaved.'           )


      CALL ZZINILNK ( MAXA, MXCELL, NCELL, ALIST, CELLS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      AVALS(1) = 4
      AVALS(2) = 1
      AVALS(3) = 30

      N        = 20

      DO I = 1, N

         DO J = 1, NAVALS

            BVAL = ( J * 1000 ) + I

            CALL ZZADDLNK ( AVALS(J), BVAL,  MAXA, MXCELL, &
                            ALIST,    NCELL, CELLS        )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


!
!     Check NCELL.
!
      CALL CHCKSI ( 'NCELL', NCELL, '=', NAVALS*N, 0, OK )

!
!     Recover the B values for each A value.
!
      DO I = 1, NAVALS

         J    = ALIST( AVALS(I) )

         DO K = 1, N

            BVAL = CELLS( 1,J )

            XVAL = ( I * 1000 ) + ( N + 1 - K )

            LABEL = 'BLIST(@) for AVAL @'
            CALL REPMI ( LABEL, '@', K, LABEL )
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, BVAL, '=', XVAL, 0, OK )


            J    = CELLS( 2,J )

         END DO

      END DO



!***********************************************************************
!
!     ZZTRVLNK tests
!
!***********************************************************************

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Collect the B-lists for AVALS from the previous ' &
      //           'ZZADDLNK case.'                                  )


      DO I = 1, NAVALS

         CALL ZZTRVLNK ( AVALS(I), MAXA, ALIST, MXCELL, &
                         CELLS,    MAXB, NB,    BLIST   )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check size of returned list.
!
         CALL CHCKSI ( 'NB', NB, '=', N, 0, OK )

!
!        Check the list itself.
!
         DO K = 1, N

            XVAL = ( I * 1000 ) + ( N + 1 - K )

            LABEL = 'BLIST(@) for AVAL @'
            CALL REPMI ( LABEL, '@', K, LABEL )
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, BLIST(K), '=', XVAL, 0, OK )

         END DO

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Pointer for AVAL is null.' )

      I = -1
      CALL ZZTRVLNK ( 1,     MAXA, I,  MXCELL, &
                      CELLS, MAXB, NB, BLIST   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check size of returned list.
!
      CALL CHCKSI ( 'NB', NB, '=', 0, 0, OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Pointer for AVAL is out of range.' )

      I = -2
      CALL ZZTRVLNK ( 1,     MAXA, I,  MXCELL, &
                      CELLS, MAXB, NB, BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTEROUTOFRANGE)', OK )

      I =  0
      CALL ZZTRVLNK ( 1,     MAXA, I,  MXCELL, &
                      CELLS, MAXB, NB, BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTEROUTOFRANGE)', OK )

      I = MXCELL + 1
      CALL ZZTRVLNK ( 1,     MAXA, I,  MXCELL, &
                      CELLS, MAXB, NB, BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(POINTEROUTOFRANGE)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'AVAL out of range.'  )

      CALL ZZTRVLNK ( 0,     MAXA, ALIST, MXCELL, &
                      CELLS, MAXB, NB,    BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(INDEXOUTOFRANGE)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'MAXB out of range.'  )

      CALL ZZTRVLNK ( 1,     MAXA, ALIST, MXCELL, &
                      CELLS, 0,    NB,    BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )


! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'BLIST too large'  )

      CALL ZZTRVLNK ( AVALS(1), MAXA, ALIST, MXCELL, &
                      CELLS,    N-1,  NB,    BLIST   )
      CALL CHCKXC ( .TRUE., 'SPICE(BARRAYTOOSMALL)', OK )



!***********************************************************************
!
!     ZZUNTNGL tests
!
!***********************************************************************

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Untangle the interleaved list structure from the ' &
      //           'last ZZADDLNK case.'                              )


      CALL ZZUNTNGL ( MAXA,  MXCELL, CELLS, MAXB, &
                      ALIST, NB,     BLIST        )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Check size of returned array.
!
      CALL CHCKSI ( 'NB', NB, '=', NAVALS * (N+1), 0, OK )


!
!     Check the array itself.
!
      DO I = 1, NAVALS
!
!        Let J be the start index of the Ith list. A count
!        should be present at that index.
!
         J = ALIST( AVALS(I) )

         LABEL = 'Count for AVAL @'
         CALL REPMI ( LABEL, '@', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, BLIST(J), '=', N, 0, OK )

!
!        Check the B-values for the Ith list.
!
         DO K = 1, N

            XVAL = ( I * 1000 ) + ( N + 1 - K )

            LABEL = 'BLIST(@) for AVAL @'
            CALL REPMI ( LABEL, '@', K, LABEL )
            CALL REPMI ( LABEL, '@', I, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, BLIST(J+K), '=', XVAL, 0, OK )

         END DO

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Output array size too small.'  )


      CALL ZZUNTNGL ( MAXA,  MXCELL, CELLS, 1, &
                      ALIST, NB,     BLIST        )
      CALL CHCKXC ( .TRUE., 'SPICE(BARRAYTOOSMALL)', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
