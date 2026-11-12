!$Procedure      F_ZZVRTPLT ( Test ZZVRTPLT )

      SUBROUTINE F_ZZVRTPLT ( OK )
      use pepper_legacy
      use pepper_inc_dsk02

! $ Abstract
!
!     Test the private SPICELIB routine ZZVRTPLT.
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
!     This routine tests the private routine ZZVRTPLT.
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
!     W.L. Taber       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 20-MAY-2016  (NJB)
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
      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 40 )

      INTEGER               MAXV
      PARAMETER           ( MAXV   = 10000 )

      INTEGER               MAXP
      PARAMETER           ( MAXP   = 2 * MAXV )

      INTEGER               MXCEL
      PARAMETER           ( MXCEL  = 3*MAXP  )

      INTEGER               MXLIST
      PARAMETER           ( MXLIST = 3*MAXP + MAXV )

!
!     Local Variables
!
      CHARACTER*(LBLSIZ)    LABEL

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      VERTS  ( 3, MAXV  )

      INTEGER               CELLS  ( 2, MXCEL )
      INTEGER               CELSIZ
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               PLATES ( 3, MAXP  )
      INTEGER               MAXLST
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLIST
      INTEGER               NLON
      INTEGER               NP
      INTEGER               NV
      INTEGER               P
      INTEGER               PLT
      INTEGER               PLTLST ( MXCEL )
      INTEGER               VRTPTR ( MAXV  )
      INTEGER               VTX

      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZVRTPLT' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Set-up: create plates for a tessellated ' &
      //           'ellipsoid. NLON = 100; NLAT = 50; NV = ' &
      //           '4902; NP = 9800.'                       )

      A = 3000.D0
      B = 2000.D0
      C = 1000.D0

      NLON = 100
      NLAT = 50

      CALL ZZELLPLT ( A,    B,  C,     NLON, NLAT,  MAXV, &
                      MAXP, NV, VERTS, NP,   PLATES      )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Create vertex-plate mapping for tessellated ' &
      //           'ellipsoid.'                                  )

      MAXLST = MXLIST

      CALL ZZVRTPLT ( NV,    NP,     PLATES, MXCEL, MAXLST, &
                      CELLS, VRTPTR, NLIST,  PLTLST        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The output list size should be exactly
!
!        NV  +  3*NP
!
!     since each plate is on exactly three vertex lists, and
!     since there is one plate count per vertex.
!
      CALL CHCKSI ( 'NLIST', NLIST, '=', NV+3*NP, 0, OK )

!
!     Each vertex pointer should be in the range 1:NLIST-1.
!
      DO I = 1, NV

         LABEL = 'VRTPTR(#)'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, VRTPTR, '>=', 1,       0, OK )
         CALL CHCKSI ( LABEL, VRTPTR, '<=', NLIST-1, 0, OK )

      END DO

!
!     Given the tiling pattern, except for the polar vertices,
!     each vertex's plate count should be in the range 1:6.
!
      DO I = 1, NV-2

         N = PLTLST( VRTPTR(I) )
         LABEL = 'Plate count(#)'
         CALL REPMI ( LABEL, '#', I, LABEL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( LABEL, N, '>=', 1, 0, OK )
         CALL CHCKSI ( LABEL, N, '<=', 6, 0, OK )

      END DO

!
!     The polar vertices are shared by NLON plates each.
!
      I = NV-1

      N = PLTLST( VRTPTR(I) )
      LABEL = 'Plate count(#)'
      CALL REPMI ( LABEL, '#', I, LABEL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( LABEL, N, '=', NLON, 0, OK )

      I = NV

      N = PLTLST( VRTPTR(I) )
      LABEL = 'Plate count(#)'
      CALL REPMI ( LABEL, '#', I, LABEL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( LABEL, N, '=', NLON, 0, OK )

!
!     Make sure each plate is on the plate lists of
!     its three vertices.
!
      DO I = 1, NP

         DO J = 1, 3
!
!           P is the pointer from vertex J of plate I
!           into the plate list. N is the plate count
!           for that vertex.
!
            VTX = PLATES(J,I)

            P   = VRTPTR( VTX )

            N   = PLTLST( P )

            FOUND = .FALSE.
            K     = 0

            DO WHILE ( ( K .LT. N ) .AND. ( .NOT. FOUND ) )
!
!              Examine the next plate in the list of the
!              current vertex.
!
               K     = K + 1

               PLT   = PLTLST( P + K )

               FOUND = ( PLT .EQ. I )

            END DO


            LABEL = 'Entry for plate(#), vertex(#)'

            CALL REPMI ( LABEL, '#', I,   LABEL )
            CALL REPMI ( LABEL, '#', VTX, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

         END DO

      END DO

!
!     If the last test passed, each plate is on at least the three
!     plate lists of its own vertices. The plate is not on any other
!     vertex's list, since total entry count is correct. The plate list
!     is valid.
!

!***********************************************************************
!
!     Error cases
!
!***********************************************************************

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad plate count.'  )

      MAXLST = MXCEL

      CALL ZZVRTPLT ( NV,    0,      PLATES, MXCEL, MAXLST, &
                      CELLS, VRTPTR, NLIST,  PLTLST        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADPLATECOUNT)', OK )


      CALL ZZVRTPLT ( NV,    -1,     PLATES, MXCEL, MAXLST, &
                      CELLS, VRTPTR, NLIST,  PLTLST        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADPLATECOUNT)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Bad vertex count.'  )

      MAXLST = MXCEL

      CALL ZZVRTPLT ( 0,     NP,     PLATES, MXCEL, MAXLST, &
                      CELLS, VRTPTR, NLIST,  PLTLST        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADVERTEXCOUNT)', OK )


      CALL ZZVRTPLT ( -1,    NP,     PLATES, MXCEL, MAXLST, &
                      CELLS, VRTPTR, NLIST,  PLTLST        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADVERTEXCOUNT)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Cell array too small.'  )

      MAXLST = MXCEL

      CELSIZ = 3*NP - 1

      CALL ZZVRTPLT ( NV,    NP,     PLATES, CELSIZ, MAXLST, &
                      CELLS, VRTPTR, NLIST,  PLTLST         )
      CALL CHCKXC ( .TRUE., 'SPICE(CELLARRAYTOOSMALL)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Plate list too small.'  )


      MAXLST = 3*NP + NV - 1

      CALL ZZVRTPLT ( NV,    NP,     PLATES, MXCEL, MAXLST, &
                      CELLS, VRTPTR, NLIST,  PLTLST         )
      CALL CHCKXC ( .TRUE., 'SPICE(PLATELISTTOOSMALL)', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
