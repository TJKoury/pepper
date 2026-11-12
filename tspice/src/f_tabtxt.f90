!$Procedure F_TABTXT ( Text kernel tab processing tests )

      SUBROUTINE F_TABTXT ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise tab processing in entry points of RDKER.
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
!     This file creates both normal and a tab-filled text
!     kernels for testing. Both are deleted at the end
!     of execution.
!
! $ Particulars
!
!     This routine tests the ability of the RDKER entry points
!
!        RDKNEW
!        RDKDAT
!
!     to process kernels where either blanks or tab characters
!     are used, in any combination, as white space.
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
!-    TSPICE Version 1.1.0, 12-OCT-2021 (NJB)
!
!        Cell arrays are now completely initialized, in order to
!        pacify Valgrind.
!
!-    TSPICE Version 1.0.0, 21-FEB-2008 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Non-SPICELIB functions
!
      CHARACTER*(20)        BEGDAT
      CHARACTER*(20)        BEGTXT

!
!     Local Parameters
!
      CHARACTER*(*)         TXT
      PARAMETER           ( TXT    = 'test.ker' )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               TABCDE
      PARAMETER           ( TABCDE = 9 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 60 )

      INTEGER               MAXBUF
      PARAMETER           ( MAXBUF = 21 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    FILBUF ( LBCELL : MAXBUF )
      CHARACTER*(LNSIZE)    TABBUF ( LBCELL : MAXBUF )
      CHARACTER*(LNSIZE)    KV1    ( 3 )
      CHARACTER*(LNSIZE)    XCVAR  ( 3 )

      DOUBLE PRECISION      KV2    ( 3 )
      DOUBLE PRECISION      KV3    ( 3 )
      DOUBLE PRECISION      XDPVAR ( 3 )

      INTEGER               I
      INTEGER               N
      INTEGER               UNIT

      LOGICAL               FOUND

!
!     Open the test family.
!
      CALL TOPEN ( 'F_TABTXT' )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Init: create a normal text kernel file.' )

!
!     Initialize cell arrays, so Valgrind won't complain about
!     copying cells.
!
      DO I = LBCELL, MAXBUF
         FILBUF(I) = ' '
         TABBUF(I) = ' '
      END DO

!
!     Rather than create a text kernel using an existing utility
!     routine, we create the file in-line so that we can ensure
!     the file has an instance of each structural feature.
!
      CALL SSIZEC ( MAXBUF, FILBUF )

      FILBUF(1)  = ' '
      FILBUF(2)  = ' KPL/PCK'
      FILBUF(3)  = ' '
      FILBUF(4)  = ' '//BEGDAT()
      FILBUF(5)  = ' '
      FILBUF(6)  = ' KV1 = ( '' A '', '' B '', '' C '' )'
      FILBUF(7)  = ' KV2 = ( 1.0, 2.0, 3.0 )'
      FILBUF(8)  = ' '
      FILBUF(9)  = ' '//BEGTXT()
      FILBUF(10) = ' '
      FILBUF(11) = ' Comment line 1'
      FILBUF(12) = ' Comment line 2'
      FILBUF(13) = ' Comment line 3'
      FILBUF(14) = ' '
      FILBUF(15) = ' '//BEGDAT()
      FILBUF(16) = ' KV3 = ( -1.0, -2.0, '
      FILBUF(17) = ' '
      FILBUF(18) = ' -3.0 )'
      FILBUF(19) = ' '
      FILBUF(20) = ' '//BEGTXT()
      FILBUF(21) = ' '

      CALL SCARDC ( MAXBUF, FILBUF )

!
!     Open and write the file.
!
      CALL TXTOPN ( TXT, UNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         I = 1

         DO WHILE ( OK .AND. ( I .LE. CARDC(FILBUF) ) )

            CALL WRITLN ( FILBUF(I), UNIT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            I = I + 1

         END DO

      END IF

      CLOSE ( UNIT )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Control case: load normal file and check.' )

      CALL LDPOOL ( TXT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the string variable KV1.
!
      CALL GCPOOL ( 'KV1', 1, 3, N, KV1, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN

         CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

         XCVAR(1) = ' A'
         XCVAR(2) = ' B'
         XCVAR(3) = ' C'

         CALL CHCKAC ( 'KV1', KV1, '=', XCVAR, 3, OK )

      END IF

!
!     Check the d.p. variable KV2.
!
      CALL GDPOOL ( 'KV2', 1, 3, N, KV2, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN

         CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

         XDPVAR(1) = 1.D0
         XDPVAR(2) = 2.D0
         XDPVAR(3) = 3.D0

         CALL CHCKAD ( 'KV2', KV2, '=', XDPVAR, 3, 0.D0, OK )

      END IF


!
!     Check the d.p. variable KV3.
!
      CALL GDPOOL ( 'KV3', 1, 3, N, KV3, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN

         CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

         XDPVAR(1) = -1.D0
         XDPVAR(2) = -2.D0
         XDPVAR(3) = -3.D0

         CALL CHCKAD ( 'KV3', KV3, '=', XDPVAR, 3, 0.D0, OK )

      END IF




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Money case: load tab-ridden file and check.' )

!
!     TABBUF will contain the same string data as FILBUF, except
!     that every blank in FILBUF is replaced by a tab character.
!
      CALL SSIZEC ( MAXBUF, TABBUF )

      CALL COPYC  ( FILBUF, TABBUF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CARDC(TABBUF)

         CALL REPLCH ( TABBUF(I), ' ', CHAR(TABCDE), TABBUF(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        The following lines of code, which are intentionally commented
!        out, can be used to verify that tab characters have been
!        inserted where expected. This can be useful because
!        applications one might use to view the file we're creating
!        don't necessarily display all tab characters as a fixed set of
!        consecutive blank spaces. For example, the programs cat, more,
!        vi, and emacs, at least as configured on the author's
!        computer, all show a single blank space between the words
!        "Comment" and "line", even though a tab character is actually
!        present between them, while other tabs are shown as multiple
!        consecutive blanks.
!
!         CALL REPLCH ( TABBUF(I), CHAR(TABCDE), '@', TABBUF(I) )
!         WRITE (*,*) TABBUF(I)
!         CALL REPLCH ( TABBUF(I), '@', CHAR(TABCDE), TABBUF(I) )
!
!
!        The output created by these lines is shown below.
!
!           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @KPL/PCK@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @\begindata@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @KV1@=@(@'@A@',@'@B@',@'@C@'@)@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @KV2@=@(@1.0,@2.0,@3.0@)@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @\begintext@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @Comment@line@1@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @Comment@line@2@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @Comment@line@3@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @\begindata@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @KV3@=@(@-1.0,@-2.0,@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @-3.0@)@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @\begintext@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!

      END DO

!
!     Empty the kernel pool. Delete the normal kernel.
!
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( TXT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open and write the new file containing tabs.
!
      CALL TXTOPN ( TXT, UNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( OK ) THEN

         I = 1

         DO WHILE ( OK .AND. ( I .LE. CARDC(TABBUF) ) )

            CALL WRITLN ( TABBUF(I), UNIT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            I = I + 1

         END DO

      END IF

      CLOSE ( UNIT )

!
!     Load the new kernel. We don't expect to hit any
!     SPICE errors, even though the file contains tabs.
!
      CALL LDPOOL ( TXT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the string variable KV1.
!
      CALL GCPOOL ( 'KV1', 1, 3, N, KV1, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN

         CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

         XCVAR(1) = ' A'
         XCVAR(2) = ' B'
         XCVAR(3) = ' C'

!
!        On some systems, each tab in an input text line will be
!        replaced by one or more blanks on input, before RDKER has a
!        chance to process the line. This can cause the values
!        of the string variables to have extra leading blanks.
!        Compress out any such blanks.
!
         DO I = 1, 3
            CALL CMPRSS ( ' ', 1, KV1(I), KV1(I) )
         END DO

         CALL CHCKAC ( 'KV1', KV1, '=', XCVAR, 3, OK )

      END IF


!
!     Check the d.p. variable KV2.
!
      CALL GDPOOL ( 'KV2', 1, 3, N, KV2, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN

         CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

         XDPVAR(1) = 1.D0
         XDPVAR(2) = 2.D0
         XDPVAR(3) = 3.D0

         CALL CHCKAD ( 'KV2', KV2, '=', XDPVAR, 3, 0.D0, OK )

      END IF


!
!     Check the d.p. variable KV3.
!
      CALL GDPOOL ( 'KV3', 1, 3, N, KV3, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN

         CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

         XDPVAR(1) = -1.D0
         XDPVAR(2) = -2.D0
         XDPVAR(3) = -3.D0

         CALL CHCKAD ( 'KV3', KV3, '=', XDPVAR, 3, 0.D0, OK )

      END IF

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up: delete text kernel file.' )


      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( TXT )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )
      RETURN
      END
