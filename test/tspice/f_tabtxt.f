C$Procedure F_TABTXT ( Text kernel tab processing tests )

      SUBROUTINE F_TABTXT ( OK )

C$ Abstract
C
C     Exercise tab processing in entry points of RDKER.
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
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     This file creates both normal and a tab-filled text
C     kernels for testing. Both are deleted at the end
C     of execution.
C
C$ Particulars
C
C     This routine tests the ability of the RDKER entry points
C
C        RDKNEW
C        RDKDAT
C
C     to process kernels where either blanks or tab characters
C     are used, in any combination, as white space.
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 12-OCT-2021 (NJB)
C
C        Cell arrays are now completely initialized, in order to
C        pacify Valgrind.
C
C-    TSPICE Version 1.0.0, 21-FEB-2008 (NJB)
C
C-&

C
C     SPICELIB functions
C
      INTEGER               CARDC

C
C     Non-SPICELIB functions
C
      CHARACTER*(20)        BEGDAT
      CHARACTER*(20)        BEGTXT

C
C     Local Parameters
C
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

C
C     Local Variables
C
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

C
C     Open the test family.
C
      CALL TOPEN ( 'F_TABTXT' )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Init: create a normal text kernel file.' )

C
C     Initialize cell arrays, so Valgrind won't complain about
C     copying cells.
C
      DO I = LBCELL, MAXBUF
         FILBUF(I) = ' '
         TABBUF(I) = ' '
      END DO
      
C
C     Rather than create a text kernel using an existing utility
C     routine, we create the file in-line so that we can ensure
C     the file has an instance of each structural feature.
C
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

C
C     Open and write the file.
C
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

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Control case: load normal file and check.' )

      CALL LDPOOL ( TXT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the string variable KV1.
C
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

C
C     Check the d.p. variable KV2.
C
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


C
C     Check the d.p. variable KV3.
C
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




C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Money case: load tab-ridden file and check.' )

C
C     TABBUF will contain the same string data as FILBUF, except
C     that every blank in FILBUF is replaced by a tab character.
C
      CALL SSIZEC ( MAXBUF, TABBUF )

      CALL COPYC  ( FILBUF, TABBUF )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, CARDC(TABBUF)

         CALL REPLCH ( TABBUF(I), ' ', CHAR(TABCDE), TABBUF(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        The following lines of code, which are intentionally commented
C        out, can be used to verify that tab characters have been
C        inserted where expected. This can be useful because
C        applications one might use to view the file we're creating
C        don't necessarily display all tab characters as a fixed set of
C        consecutive blank spaces. For example, the programs cat, more,
C        vi, and emacs, at least as configured on the author's
C        computer, all show a single blank space between the words
C        "Comment" and "line", even though a tab character is actually
C        present between them, while other tabs are shown as multiple
C        consecutive blanks.
C
C         CALL REPLCH ( TABBUF(I), CHAR(TABCDE), '@', TABBUF(I) )
C         WRITE (*,*) TABBUF(I)
C         CALL REPLCH ( TABBUF(I), '@', CHAR(TABCDE), TABBUF(I) )
C
C
C        The output created by these lines is shown below.
C
C           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @KPL/PCK@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @\begindata@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @KV1@=@(@'@A@',@'@B@',@'@C@'@)@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @KV2@=@(@1.0,@2.0,@3.0@)@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @\begintext@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @Comment@line@1@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @Comment@line@2@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @Comment@line@3@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @\begindata@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @KV3@=@(@-1.0,@-2.0,@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @-3.0@)@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @\begintext@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C           @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C

      END DO

C
C     Empty the kernel pool. Delete the normal kernel.
C
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( TXT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Open and write the new file containing tabs.
C
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

C
C     Load the new kernel. We don't expect to hit any
C     SPICE errors, even though the file contains tabs.
C
      CALL LDPOOL ( TXT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the string variable KV1.
C
      CALL GCPOOL ( 'KV1', 1, 3, N, KV1, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN

         CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

         XCVAR(1) = ' A'
         XCVAR(2) = ' B'
         XCVAR(3) = ' C'

C
C        On some systems, each tab in an input text line will be
C        replaced by one or more blanks on input, before RDKER has a
C        chance to process the line. This can cause the values
C        of the string variables to have extra leading blanks.
C        Compress out any such blanks.
C
         DO I = 1, 3
            CALL CMPRSS ( ' ', 1, KV1(I), KV1(I) )
         END DO

         CALL CHCKAC ( 'KV1', KV1, '=', XCVAR, 3, OK )

      END IF


C
C     Check the d.p. variable KV2.
C
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


C
C     Check the d.p. variable KV3.
C
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

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up: delete text kernel file.' )


      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( TXT )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )
      RETURN
      END
