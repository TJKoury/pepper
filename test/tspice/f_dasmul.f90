!$Procedure F_DASMUL ( DAS test, multiple DAS files )

      SUBROUTINE F_DASMUL ( OK )

! $ Abstract
!
!     Exercise DAS routines using multiple DAS files.
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
!     This routine tests the ability of the DAS system to read from
!     FTSIZE open DAS files, where FTSIZE is the limit imposed by
!     the handle manager subsystem.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     This file currently open DAS files using DASOPR.
!     When the required supporting code is available, the
!     DASOPR calls can be replaced by calls to FURNSH and
!     KINFO.
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
!-    TSPICE Version 1.0.0, 11-FEB-2017 (NJB)
!
!        Removed unneeded declarations.
!
!        26-FEB-2015 (NJB)
!
!          Original version.
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               EXISTS

!
!     Local Parameters
!

      INTEGER               SCHCOD
      PARAMETER           ( SCHCOD = 2 )


      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 5000 )

      INTEGER               MAXCLU
      PARAMETER           ( MAXCLU = 1000 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 12 )

      INTEGER               CHR
      PARAMETER           ( CHR    =  1  )

      INTEGER               DP
      PARAMETER           ( DP     =  2 )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )


      INTEGER               FILEX
      PARAMETER           ( FILEX  = 100000 )

      INTEGER               RECX
      PARAMETER           ( RECX   =  1000 )


!
!     Local Variables
!
      CHARACTER*(MAXVAL)    CDATA
      CHARACTER*(FILSIZ)    DAS    ( FTSIZE )
      CHARACTER*(MAXVAL)    XCDATA

      DOUBLE PRECISION      DDATA  ( MAXVAL )
      DOUBLE PRECISION      XDDATA ( MAXVAL )

      INTEGER               CLNWDS ( MAXCLU )
      INTEGER               CLTYPS ( MAXCLU )
      INTEGER               HANDLE ( FTSIZE )
      INTEGER               I
      INTEGER               IDATA  ( MAXVAL )
      INTEGER               J
      INTEGER               K
      INTEGER               NCLUST
      INTEGER               NCOMRC
      INTEGER               NFILES
      INTEGER               NSCHEM
      INTEGER               RECNO
      INTEGER               XIDATA ( MAXVAL )

      LOGICAL               FCLOSE
      LOGICAL               SGRGAT


!
!     Saved all.
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_DASMUL' )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create DAS files.' )

      NFILES = FTSIZE

      NFILES = 5000

      NCOMRC = 2

      NCLUST = 3

      CLTYPS(1) = DP
      CLTYPS(2) = CHR
      CLTYPS(3) = INT

      CLNWDS(1) = 5
      CLNWDS(2) = 7
      CLNWDS(3) = 12

      SGRGAT    = .FALSE.
      FCLOSE    = .TRUE.
      NSCHEM    = SCHCOD

      DO I = 1, NFILES

         DAS(I) = 'mult_test_#.das'
         CALL REPMI ( DAS(I), '#', I, DAS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF (  EXISTS( DAS(I) )  ) THEN
            CALL DELFIL ( DAS(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )
         END IF

         CALL TSTDAS ( DAS(I),   'DSK',  NCOMRC, I,      NCLUST, &
                       CLTYPS,   CLNWDS, SGRGAT, FCLOSE, NSCHEM, &
                       HANDLE(I)                                 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


      DO I = 1, NFILES

         CALL DASOPR( DAS(I), HANDLE(I) )

!         CALL FURNSH ( DAS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

!         CALL KINFO ( DAS(I), FILTYP, SOURCE, HANDLE(I), FOUND )
!         CALL CHCKXC ( .FALSE., ' ', OK )
!         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      END DO

!
!     Read data from open files.
!
      DO I = 1, NFILES

         DO J = 1, NCLUST

            IF ( CLTYPS(J) .EQ. INT ) THEN

               CALL DASRDI ( HANDLE(I), 1, CLNWDS(J), IDATA )
               CALL CHCKXC ( .FALSE., ' ', OK )

               RECNO = 3

               DO K = 1, CLNWDS(J)
                  XIDATA(K) = ( I * FILEX ) + RECX*RECNO + K
               END DO

               CALL CHCKAI ( 'IDATA', IDATA,  '=', &
                                      XIDATA, CLNWDS(J), OK )


            ELSE IF ( CLTYPS(J) .EQ. DP ) THEN

               CALL DASRDD ( HANDLE(I), 1, CLNWDS(J), DDATA )
               CALL CHCKXC ( .FALSE., ' ', OK )

               RECNO = 1

               DO K = 1, CLNWDS(J)
                  XDDATA(K) = ( I * FILEX ) + RECX*RECNO + DBLE(K)
               END DO

               CALL CHCKAD ( 'DDATA', DDATA,  '=', &
                                      XDDATA, CLNWDS(J), 0.D0, OK )

            ELSE

               CALL DASRDC ( HANDLE(I), 1, CLNWDS(J), 1, MAXVAL, CDATA )
               CALL CHCKXC ( .FALSE., ' ', OK )

               RECNO = 2

               DO K = 1, CLNWDS(J)

                  XIDATA(K)   = ( I * FILEX ) + RECX*RECNO + K

                  XCDATA(K:K) = CHAR( MOD( XIDATA(K), 128 ) )

               END DO

               CALL CHCKSC ( 'CDATA', CDATA, '=', XCDATA, OK )

            END IF

         END DO

      END DO

      DO I = 1, NFILES

         CALL DASCLS ( HANDLE(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up: delete DAS files.' )


      DO I = 1, NFILES

         CALL DELFIL ( DAS(I)    )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO
!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
