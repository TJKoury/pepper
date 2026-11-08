!$Procedure      F_DSKOBJ ( Test DSK body/surface coverage routines )

      SUBROUTINE F_DSKOBJ ( OK )
      IMPLICIT NONE

! $ Abstract
!
!     Test the SPICELIB DSK body/surface coverage routines.
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

      INCLUDE 'srftrn.inc'
      INCLUDE 'zzctr.inc'
      INCLUDE 'zzsrftrn.inc'

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
!     This routine tests the DSK body/surface coverage routines
!
!        DSKOBJ
!        DSKSRF
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
!-    TSPICE Version 1.0.0, 16-MAY-2016 (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB functions
!
      INTEGER               CARDI
      INTEGER               ISRCHI

      LOGICAL               EXISTS

!
!     Local parameters
!
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'dskobj_test0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1   = 'dskobj_test1.bds' )

      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0   = 'dskobj_test0.bsp' )

      CHARACTER*(*)         XFR0
      PARAMETER           ( XFR0   = 'dskobj_test0.xfr' )

      CHARACTER*(*)         EK0
      PARAMETER           ( EK0    = 'dskobj_test0.bes' )


      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               NBOD
      PARAMETER           ( NBOD   = 10 )

      INTEGER               NDSK0
      PARAMETER           ( NDSK0  =  10 )

      INTEGER               NDSK1
      PARAMETER           ( NDSK1  = 1000 )

      INTEGER               MAXSIZ
      PARAMETER           ( MAXSIZ = 5000 )

!
!     Local Variables
!
      CHARACTER*(FRNMLN)    FRNAME

      INTEGER               BODSET ( LBCELL : MAXSIZ )
      INTEGER               BODLST ( NBOD )
      INTEGER               BODYID
      INTEGER               FRAMID
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               N
      INTEGER               SRFSET ( LBCELL : MAXSIZ )
      INTEGER               UNIT
      INTEGER               XBDSET ( LBCELL : MAXSIZ )
      INTEGER               XN
      INTEGER               XSFSET ( LBCELL : MAXSIZ )

      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_DSKOBJ' )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Setup: create DSK files' )

!
!     Start fresh. Since we're going to create a DSK and
!     then append to it, we don't want to create an ever-growing
!     monster file.
!
      IF ( EXISTS(DSK0) ) THEN

         CALL DELFIL( DSK0 )
         CALL CHCKXC( .FALSE., ' ', OK )

      END IF

!
!     Create a body list.
!
      DO I = 1, NBOD-1
         BODLST(I) = 100*I + 99
      END DO

      BODLST(NBOD) = 10

!
!     Create a DSK file containing NDSK0 segments.
!
      DO I = 1, NDSK0
!
!        Generate a body ID for the Ith segment. Pick
!        a body from the body list. Recall we need to
!        restrict the body to the set for which there
!        are built-in body-fixed frame definitions.
!
         J      = 1 + MOD( I-1, NBOD )
         BODYID = BODLST(J)

         CALL CIDFRM ( BODYID, FRAMID, FRNAME, FOUND )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         CALL T_SMLDSK( BODYID, I, FRNAME, DSK0 )
         CALL CHCKXC( .FALSE., ' ', OK )

      END DO

!
!     Create a DSK file containing NDSK1 segments.
!
      IF ( EXISTS(DSK1) ) THEN

         CALL DELFIL( DSK1 )
         CALL CHCKXC( .FALSE., ' ', OK )

      END IF

      DO I = 1, NDSK1
!
!        Generate a body ID for the Ith segment. Pick
!        a body from the body list. Recall we need to
!        restrict the body to the set for which there
!        are built-in body-fixed frame definitions.
!
         J      = 1 + MOD( I-1, NBOD )
         BODYID = BODLST(J)

         CALL CIDFRM ( BODYID, FRAMID, FRNAME, FOUND )
         CALL CHCKXC( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         CALL T_SMLDSK( BODYID, -I, FRNAME, DSK1 )
         CALL CHCKXC( .FALSE., ' ', OK )

      END DO


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check the body set for DSK0.' )

!
!     Create the expected body set.
!
      CALL SSIZEI ( MAXSIZ, XBDSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      DO I = 1, NBOD
         CALL INSRTI ( BODLST(I), XBDSET )
         CALL CHCKXC( .FALSE., ' ', OK )
      END DO

!
!     Get the body set for DSK0.
!
      CALL SSIZEI ( MAXSIZ, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKOBJ ( DSK0, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Check set cardinality.
!
      N  = CARDI( BODSET )
      XN = NBOD

      CALL CHCKSI ( 'CARDI(BODSET)', N, '=', XN, 0, OK )

      IF ( OK ) THEN
!
!        Check the body set.
!
         CALL CHCKAI ( 'BODSET', BODSET(1), '=', &
                                 XBDSET(1), XN,  OK )
      END IF


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check the body set for DSK1.' )

!
!     Create the expected body set.
!
      CALL SSIZEI ( MAXSIZ, XBDSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      DO I = 1, NBOD
         CALL INSRTI ( BODLST(I), XBDSET )
         CALL CHCKXC( .FALSE., ' ', OK )
      END DO

!
!     Get the body set for DSK1.
!
      CALL SSIZEI ( MAXSIZ, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKOBJ ( DSK1, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Check set cardinality.
!
      N  = CARDI( BODSET )
      XN = NBOD

      CALL CHCKSI ( 'CARDI(BODSET)', N, '=', XN, 0, OK )

      IF ( OK ) THEN
!
!        Check the body set.
!
         CALL CHCKAI ( 'BODSET', BODSET(1), '=', &
                                 XBDSET(1), XN,  OK )
      END IF



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check the surface set for each body in DSK0.' )

      CALL SSIZEI ( MAXSIZ, SRFSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL SSIZEI ( MAXSIZ, XSFSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL SCARDI ( 0, BODSET )

      CALL DSKOBJ ( DSK0, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      DO I = 1, CARDI( BODSET )

         CALL SCARDI( 0, SRFSET )
         CALL CHCKXC( .FALSE., ' ', OK )
!
!        Create the expected surface set.
!
         CALL SCARDI( 0, XSFSET )
         CALL CHCKXC( .FALSE., ' ', OK )

         BODYID = BODSET(I)

         J      = ISRCHI ( BODYID, NBOD, BODLST )
         K      = J

         DO WHILE ( K .LE. NDSK0 )

            CALL INSRTI ( K, XSFSET )
            CALL CHCKXC( .FALSE., ' ', OK )

            K = K + NBOD

         END DO

         XN = CARDI( XSFSET )


         CALL DSKSRF ( DSK0, BODSET(I), SRFSET )
         CALL CHCKXC( .FALSE., ' ', OK )

!
!        Check the cardinality of the surface set.
!
         N = CARDI( SRFSET )

         CALL CHCKSI ( 'CARDI(SRFSET)', N, '=', XN, 0, OK )

!
!        Check the surface set itself.
!
         IF ( OK ) THEN

            CALL CHCKAI ( 'SRFSET', SRFSET(1), '=', &
                                    XSFSET(1), XN,  OK )
         END IF

      END DO

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Check the surface set for each body in DSK1.' )

      CALL SSIZEI ( MAXSIZ, SRFSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL SSIZEI ( MAXSIZ, XSFSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL SCARDI ( 0, BODSET )

      CALL DSKOBJ ( DSK1, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      DO I = 1, CARDI( BODSET )

         CALL SCARDI( 0, SRFSET )
         CALL CHCKXC( .FALSE., ' ', OK )
!
!        Create the expected surface set.
!
         CALL SCARDI( 0, XSFSET )
         CALL CHCKXC( .FALSE., ' ', OK )

         BODYID = BODSET(I)

         J      = ISRCHI ( BODYID, NBOD, BODLST )
         K      = J

         DO WHILE ( K .LE. NDSK1 )

            CALL INSRTI ( -K, XSFSET )
            CALL CHCKXC( .FALSE., ' ', OK )

            K = K + NBOD

         END DO

         XN = CARDI( XSFSET )


         CALL DSKSRF ( DSK1, BODSET(I), SRFSET )
         CALL CHCKXC( .FALSE., ' ', OK )

!
!        Check the cardinality of the surface set.
!
         N = CARDI( SRFSET )

         CALL CHCKSI( 'CARDI(SRFSET)', N, '=', XN, 0, OK )

!
!        Check the surface set itself.
!
         IF ( OK ) THEN

            CALL CHCKAI ( 'SRFSET', SRFSET(1), '=', &
                                    XSFSET(1), XN,  OK )
         END IF

      END DO



!***********************************************************************
!
!
!     DSKOBJ error cases
!
!
!***********************************************************************


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DSKOBJ: output cell is too small.' )

      CALL SSIZEI( 3, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKOBJ ( DSK0, BODSET )
      CALL CHCKXC ( .TRUE., 'SPICE(CELLTOOSMALL)', OK )

!
!     This call will leave the DSK closed.
!

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DSKOBJ: DSK doesn''t exist.' )

      CALL SCARDI ( 0, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKOBJ ( 'XXX', BODSET )
      CALL CHCKXC( .TRUE., 'SPICE(FILENOTFOUND)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DSKOBJ: file is a non-DSK DAS.' )

      CALL TSTEK ( EK0, 1, 10, .FALSE., HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL SCARDI ( 0, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKOBJ ( EK0, BODSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DSKOBJ: file is a DAF.' )

      CALL TSTSPK ( SPK0, .FALSE., .TRUE. )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKOBJ ( SPK0, BODSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DSKOBJ: file is an XFR file.' )

      IF ( EXISTS(XFR0) ) THEN

         CALL DELFIL( XFR0 )
         CALL CHCKXC( .FALSE., ' ', OK )

      END IF

      CALL TXTOPN( XFR0, UNIT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DASBT ( DSK0, UNIT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CLOSE ( UNIT )

      CALL DSKOBJ ( XFR0, BODSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDFORMAT)', OK )



!***********************************************************************
!
!
!     DSKSRF error cases
!
!
!***********************************************************************


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DSKSRF: DSK doesn''t exist.' )

      CALL SCARDI ( 0, SRFSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKSRF ( 'XXX', 1, SRFSET )
      CALL CHCKXC( .TRUE., 'SPICE(FILENOTFOUND)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DSKSRF: file is a non-DSK DAS.' )


      CALL DSKSRF ( EK0, 1, SRFSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DSKSRF: file is a DAF.' )

      CALL DSKSRF ( SPK0, 1, SRFSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DSKSRF: file is an XFR file.' )

      CALL DSKSRF ( XFR0, 1, BODSET )
      CALL CHCKXC( .TRUE., 'SPICE(INVALIDFORMAT)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'DSKSRF: output cell is too small.' )

      CALL SSIZEI( 3, BODSET )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DSKSRF ( DSK1, 199, BODSET )
      CALL CHCKXC ( .TRUE., 'SPICE(CELLTOOSMALL)', OK )

!
!     This call will leave the DSK closed.
!


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Clean up: delete kernels.' )


      CALL DELFIL ( SPK0 )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DELFIL ( DSK0 )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DELFIL ( DSK1 )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DELFIL ( XFR0 )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL DELFIL ( EK0 )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
