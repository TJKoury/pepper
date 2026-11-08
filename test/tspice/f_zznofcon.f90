!$Procedure F_ZZNOFCON ( ZZNOFCON tests )

      SUBROUTINE F_ZZNOFCON ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine ZZNOFCON.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      INCLUDE               'errhnd.inc'
      INCLUDE               'frmtyp.inc'

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
!     This routine tests the SPICELIB routine ZZNOFCON. ZZNOFCON
!     creates a long error message for the SPICE(NOFRAMECONNECT)
!     error.
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
!-    TSPICE Version 2.1.0, 12-MAY-2016 (EDW)
!
!        Added KCLEAR call and end of fire to clear kernel pool
!        of the loaded CK
!
!-    TSPICE Version 2.0.0, 09-SEP-2013 (NJB)
!
!        Expected long messages were updated to
!        reflect new long message text. The text
!        mentions the possibility of CK lookup
!        failure due to the absence of required
!        angular velocity data.
!
!-    TSPICE Version 1.0.0, 26-MAR-2009 (NJB)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               RTRIM
      LOGICAL               ZZSCLK

!
!     Other functions
!

!
!     Local Parameters
!
!
!     Note: the name "CK" is already in use; it's a parameter
!     in frmtyp.inc.
!
      CHARACTER*(*)         CKER
      PARAMETER           ( CKER   = 'zznofcon.bc' )

      CHARACTER*(*)         CLKKER
      PARAMETER           ( CLKKER = 'zznofcon.tsc' )



      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 600 )

      INTEGER               NCASE
      PARAMETER           ( NCASE  =  5 )

      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 82 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 35 )

!
!     Local Variables
!
      CHARACTER*(FRNMLN)    BASES  ( NCASE  )
      CHARACTER*(LMSGLN)    CKWARN
      CHARACTER*(LMSGLN)    CLOSNG
      CHARACTER*(TIMLEN)    ETSTR
      CHARACTER*(FRNMLN)    FRAMES ( NCASE  )
      CHARACTER*(LNSIZE)    KERBUF ( BUFSIZ )
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(LMSGLN)    ERRMSG
      CHARACTER*(LMSGLN)    TIMSTR
      CHARACTER*(LMSGLN)    INSUFF
      CHARACTER*(LMSGLN)    SCWARN
      CHARACTER*(LMSGLN)    TRANS

      DOUBLE PRECISION      ET

      INTEGER               BASEID ( 2 )
      INTEGER               CENTER
      INTEGER               CLASS
      INTEGER               CLKID
      INTEGER               CLSSID
      INTEGER               FRAMID ( 2 )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               M
      INTEGER               NOSCLK
      INTEGER               PATH

      LOGICAL               FOUND
      LOGICAL               ISCK

!
!     Saved values
!
      SAVE                  BASES
      SAVE                  FRAMES
      SAVE                  KERBUF

!
!     Initial values
!
      DATA                  BASES  / 'J2000', &
                                     'BOGUS_CK_-9999', &
                                     'BOGUS_CK_-9999', &
                                     'BOGUS_CK_-10000', &
                                     'J2000'           /

      DATA                  FRAMES / 'BOGUS_TK_1', &
                                     'BOGUS_TK_2', &
                                     'BOGUS_CK_-9999', &
                                     'BOGUS_CK_-10000', &
                                     'BOGUS_TK_3'      /

!
!     The simple test cases.
!



!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZNOFCON' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Load kernels and POOL data' )

!
!     Initialize ET.
!
      ET = 0.D0

!
!     Initialize the frame kernel buffer.
!
!     Note that the CK frame BOGUS_CK_-10000 has been altered
!     so that it *doesn't* refer to the CK and SCLK IDs -10000
!     and -9, which are used by TSTCK3. This CK frame never
!     has available pointing or SCLK data.
!
      KERBUF(  1 ) = '      FRAME_BOGUS_TK_1      =  1400000'
      KERBUF(  2 ) = '      FRAME_1400000_NAME     = ''BOGUS_' &
      //             'TK_1'''
      KERBUF(  3 ) = '      FRAME_1400000_CLASS    =  4'
      KERBUF(  4 ) = '      FRAME_1400000_CLASS_ID =  1400000'
      KERBUF(  5 ) = '      FRAME_1400000_CENTER   =   400000'
      KERBUF(  6 ) = ' '
      KERBUF(  7 ) = '      OBJECT_400000_FRAME    = ''BOGUS_' &
      //             'TK_1'''
      KERBUF(  8 ) = ' '
      KERBUF(  9 ) = '      TKFRAME_BOGUS_TK_1_RELATIVE = ''B' &
      //             'OGUS_CK_-9999'''
      KERBUF( 10 ) = '      TKFRAME_BOGUS_TK_1_SPEC     = ''A' &
      //             'NGLES'''
      KERBUF( 11 ) = '      TKFRAME_BOGUS_TK_1_UNITS    = ''D' &
      //             'EGREES'''
      KERBUF( 12 ) = '      TKFRAME_BOGUS_TK_1_AXES     = ( 3' &
      //             ', 2, 3 )'
      KERBUF( 13 ) = '      TKFRAME_BOGUS_TK_1_ANGLES   = ( -' &
      //             '243.126496675,'
      KERBUF( 14 ) = '                                       ' &
      //             ' -54.657822839,'
      KERBUF( 15 ) = '                                       ' &
      //             ' 180.0 )'
      KERBUF( 16 ) = ' '
      KERBUF( 17 ) = ' '
      KERBUF( 18 ) = '      FRAME_BOGUS_TK_2      =  1400001'
      KERBUF( 19 ) = '      FRAME_1400001_NAME     = ''BOGUS_' &
      //             'TK_2'''
      KERBUF( 20 ) = '      FRAME_1400001_CLASS    =  4'
      KERBUF( 21 ) = '      FRAME_1400001_CLASS_ID =  1400001'
      KERBUF( 22 ) = '      FRAME_1400001_CENTER   =   400001'
      KERBUF( 23 ) = ' '
      KERBUF( 24 ) = '      OBJECT_400001_FRAME    = ''BOGUS_' &
      //             'TK_2'''
      KERBUF( 25 ) = ' '
      KERBUF( 26 ) = '      TKFRAME_BOGUS_TK_2_RELATIVE = ''B' &
      //             'OGUS_TK_1'''
      KERBUF( 27 ) = '      TKFRAME_BOGUS_TK_2_SPEC     = ''A' &
      //             'NGLES'''
      KERBUF( 28 ) = '      TKFRAME_BOGUS_TK_2_UNITS    = ''D' &
      //             'EGREES'''
      KERBUF( 29 ) = '      TKFRAME_BOGUS_TK_2_AXES     = ( 3' &
      //             ', 2, 3 )'
      KERBUF( 30 ) = '      TKFRAME_BOGUS_TK_2_ANGLES   = ( -' &
      //             '243.126496675,'
      KERBUF( 31 ) = '                                       ' &
      //             ' -54.657822839,'
      KERBUF( 32 ) = '                                       ' &
      //             ' 180.0 )'
      KERBUF( 33 ) = ' '
      KERBUF( 34 ) = '      FRAME_BOGUS_CK_-9999        = -99' &
      //             '99'
      KERBUF( 35 ) = '      FRAME_-9999_NAME            = ''B' &
      //             'OGUS_CK_-9999'''
      KERBUF( 36 ) = '      FRAME_-9999_CLASS           =  3'
      KERBUF( 37 ) = '      FRAME_-9999_CLASS_ID        = -99' &
      //             '99'
      KERBUF( 38 ) = '      FRAME_-9999_CENTER          = -99' &
      //             '99'
      KERBUF( 39 ) = '      CK_-9999_SCLK               = -9'
      KERBUF( 40 ) = '      OBJECT_-9999_FRAME          = ''B' &
      //             'OGUS_CK_-9999'''
      KERBUF( 41 ) = ' '
      KERBUF( 42 ) = ' '
      KERBUF( 43 ) = '      FRAME_BOGUS_CK_-10000       = -10' &
      //             '000'
      KERBUF( 44 ) = '      FRAME_-10000_NAME           = ''B' &
      //             'OGUS_CK_-10000'''
      KERBUF( 45 ) = '      FRAME_-10000_CLASS          =  3'
      KERBUF( 46 ) = '      FRAME_-10000_CLASS_ID       = -10'
      KERBUF( 47 ) = '      FRAME_-10000_CENTER         = -10' &
      //             '000'
      KERBUF( 48 ) = '      CK_-10_SCLK                 = -1'
      KERBUF( 49 ) = '      OBJECT_-10000_FRAME         = ''B' &
      //             'OGUS_CK_-10000'''
      KERBUF( 50 ) = ' '
      KERBUF( 51 ) = ' '
      KERBUF( 52 ) = '      FRAME_BOGUS_TK_3      =  1400002'
      KERBUF( 53 ) = '      FRAME_1400002_NAME     = ''BOGUS_' &
      //             'TK_3'''
      KERBUF( 54 ) = '      FRAME_1400002_CLASS    =  4'
      KERBUF( 55 ) = '      FRAME_1400002_CLASS_ID =  1400002'
      KERBUF( 56 ) = '      FRAME_1400002_CENTER   =   400002'
      KERBUF( 57 ) = ' '
      KERBUF( 58 ) = '      OBJECT_400002_FRAME    = ''BOGUS_' &
      //             'TK_3'''
      KERBUF( 59 ) = ' '
      KERBUF( 60 ) = '      TKFRAME_BOGUS_TK_3_RELATIVE = ''J' &
      //             '2000'''
      KERBUF( 61 ) = '      TKFRAME_BOGUS_TK_3_SPEC     = ''A' &
      //             'NGLES'''
      KERBUF( 62 ) = '      TKFRAME_BOGUS_TK_3_UNITS    = ''D' &
      //             'EGREES'''
      KERBUF( 63 ) = '      TKFRAME_BOGUS_TK_3_AXES     = ( 3' &
      //             ', 2, 3 )'
      KERBUF( 64 ) = '      TKFRAME_BOGUS_TK_3_ANGLES   = ( -' &
      //             '243.126496675,'
      KERBUF( 65 ) = '                                       ' &
      //             ' -54.657822839,'
      KERBUF( 66 ) = '                                       ' &
      //             ' 180.0 )'
      KERBUF( 67 ) = ' '
      KERBUF( 68 ) = '      FRAME_BOGUS_TK_4      =  1400003'
      KERBUF( 69 ) = '      FRAME_1400003_NAME     = ''BOGUS_' &
      //             'TK_4'''
      KERBUF( 70 ) = '      FRAME_1400003_CLASS    =  4'
      KERBUF( 71 ) = '      FRAME_1400003_CLASS_ID =  1400003'
      KERBUF( 72 ) = '      FRAME_1400003_CENTER   =   400003'
      KERBUF( 73 ) = ' '
      KERBUF( 74 ) = '      OBJECT_400003_FRAME    = ''BOGUS_' &
      //             'TK_4'''
      KERBUF( 75 ) = ' '
      KERBUF( 76 ) = '      TKFRAME_BOGUS_TK_4_RELATIVE = ''J' &
      //             '2000'''
      KERBUF( 77 ) = '      TKFRAME_BOGUS_TK_4_SPEC     = ''A' &
      //             'NGLES'''
      KERBUF( 78 ) = '      TKFRAME_BOGUS_TK_4_UNITS    = ''D' &
      //             'EGREES'''
      KERBUF( 79 ) = '      TKFRAME_BOGUS_TK_4_AXES     = ( 3' &
      //             ', 2, 3 )'
      KERBUF( 80 ) = '      TKFRAME_BOGUS_TK_4_ANGLES   = ( -' &
      //             '243.126496675,'
      KERBUF( 81 ) = '                                       ' &
      //             ' -54.657822839,'
      KERBUF( 82 ) = '                                       ' &
      //             ' 180.0 )'


      CALL LMPOOL ( KERBUF, BUFSIZ )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create CK and SCLK kernels. Don't load the kernels; keep
!     the SCLK kernel.
!
      CALL TSTCK3 ( CKER, CLKKER, .FALSE., .FALSE., .TRUE., HANDLE )

      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 1, NCASE
!
!        Frame and base I map to the first frame ID, base ID pair.
!
         CALL NAMFRM ( FRAMES(I), FRAMID(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL NAMFRM ( BASES(I),  BASEID(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )


         DO J = 1, NCASE
!
!           Frame and base J map to the second frame ID, base ID pair.
!
            CALL NAMFRM ( FRAMES(J), FRAMID(2) )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL NAMFRM ( BASES(J),  BASEID(2) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Set ET.
!
            ET = ( 1.D8 * I ) + J


            DO M = 1, 2
!
!              Load SCLK kernel if M is 2.
!
               IF ( M .EQ. 2 ) THEN

                  CALL FURNSH ( CLKKER )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               ELSE

                  CALL UNLOAD ( CLKKER )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL LMPOOL ( KERBUF, BUFSIZ )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END IF



!
! --- Case: ------------------------------------------------------
!

               IF ( J .NE. I ) THEN

                  TITLE = 'Fr 1: #, Bs 1: #, ' &
                  //      'Fr 2: #, Bs 2: #, ' &
                  //      'SCLK:#'

                  CALL REPMC ( TITLE, '#', FRAMES(1), TITLE )
                  CALL REPMC ( TITLE, '#', BASES(1),  TITLE )
                  CALL REPMC ( TITLE, '#', FRAMES(2), TITLE )
                  CALL REPMC ( TITLE, '#', BASES(2),  TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  IF ( M .EQ. 1 ) THEN
                     CALL REPMC ( TITLE, '#', 'N',  TITLE )
                  ELSE
                     CALL REPMC ( TITLE, '#', 'Y', TITLE )
                  END IF

                  CALL TCASE  ( TITLE )

                  CALL ZZNOFCON ( ET, &
                                  FRAMID(1), BASEID(1), &
                                  FRAMID(2), BASEID(2), &
                                  ERRMSG                )

!
!                 Indicate this is not a CK case to start. We're not
!                 yet missing SCLK or CK data.
!
                  ISCK   = .FALSE.
                  NOSCLK = 0

!
!                 Create the expected time string; if this
!                 string is at the start of the message,
!                 subtract it from the message.
!
                  TIMSTR = 'At epoch # TDB (# TDB),'

                  CALL REPMD ( TIMSTR, '#', ET, 14, TIMSTR )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL ETCAL ( ET, ETSTR )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL REPMC ( TIMSTR, '#', ETSTR, TIMSTR )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  L = RTRIM ( TIMSTR )

                  IF ( ERRMSG(1:L) .EQ. TIMSTR ) THEN

                     CALL REMSUB ( ERRMSG, 1, L, ERRMSG )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END IF

!
!                 Create the "insufficient data" string; if
!                 string is at the start of the message,
!                 subtract it from the message.
!
                  INSUFF = ' there is insufficient information ' &
                  //       'available to transform from reference ' &
                  //       'frame # (#) to reference frame # (#).'

                  CALL REPMI ( INSUFF, '#', FRAMID(1), INSUFF )
                  CALL REPMC ( INSUFF, '#', FRAMES(I), INSUFF )
                  CALL REPMI ( INSUFF, '#', FRAMID(2), INSUFF )
                  CALL REPMC ( INSUFF, '#', FRAMES(J), INSUFF )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  L = RTRIM ( INSUFF )

                  IF ( ERRMSG(1:L) .EQ. INSUFF ) THEN

                     CALL REMSUB ( ERRMSG, 1, L, ERRMSG )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END IF

!
!                 Now clean up the path-dependent message portions.
!
                  DO PATH = 1, 2

!
!                    Get info for the base frame.
!
                     CALL FRINFO ( BASEID(PATH), CENTER, CLASS, &
                                   CLSSID,       FOUND          )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSL ( 'FRINFO found', &
                                   FOUND, .TRUE., OK )

!
!                    Indicate we've got at least one CK base if the
!                    current base is type CK.
!
                     IF ( CLASS .EQ. CK ) THEN
                        ISCK = .TRUE.
                     END IF

!
!                    Let K point to the index I or J, depending
!                    on which path we're looking at.
!
                     IF ( PATH .EQ. 1 ) THEN
                        K = I
                     ELSE
                        K = J
                     END IF

!
!                    If the frame doesn't equal its base, create the
!                    transformation phrase for the first path.
!
                     IF ( FRAMID(PATH) .NE. BASEID(PATH) ) THEN

!
!                       The path contains more than one node.
!
                        TRANS = ' Frame # could be transformed to ' &
                        //      'frame # (#).'

                        CALL REPMC( TRANS, '#', FRAMES(K),    TRANS )
                        CALL REPMI( TRANS, '#', BASEID(PATH), TRANS )
                        CALL REPMC( TRANS, '#', BASES (K),    TRANS )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        L = RTRIM ( TRANS )

                        IF ( ERRMSG(1:L) .EQ. TRANS ) THEN

                           CALL REMSUB ( ERRMSG, 1, L, ERRMSG )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                        END IF

                        IF ( CLASS .EQ. CK ) THEN

!
!                          If the base frame is a CK frame, create
!                          the CK warning for this frame.
!

                           CKWARN = ' The latter is a CK frame; a ' &
                           //       'CK file containing data for ' &
                           //       'instrument or structure # at ' &
                           //       'the epoch shown above, as well ' &
                           //       'as a corresponding SCLK ' &
                           //       'kernel, must be loaded in ' &
                           //       'order to use this frame.'



                           CALL REPMI ( CKWARN, '#', CLSSID, CKWARN )

                           L = RTRIM ( CKWARN )

                           IF ( ERRMSG(1:L) .EQ. CKWARN ) THEN

                              CALL REMSUB ( ERRMSG, 1, L, ERRMSG )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                           END IF

                        END IF

                     ELSE

!
!                       The path is a singleton.
!
                        IF ( CLASS .EQ. CK ) THEN

!
!                          If the base frame is a CK frame, create
!                          the CK warning for this frame.
!
                           CKWARN = ' # is a CK frame; a CK ' &
                           //       'file containing data for ' &
                           //       'instrument or structure # at ' &
                           //       'the epoch shown above, as well ' &
                           //       'as a corresponding SCLK ' &
                           //       'kernel, must be loaded in ' &
                           //       'order to use this frame.'

                           CALL REPMC( CKWARN, '#', BASES(K), CKWARN)
                           CALL REPMI( CKWARN, '#', CLSSID,   CKWARN)

                           L = RTRIM ( CKWARN )

                           IF ( ERRMSG(1:L) .EQ. CKWARN ) THEN

                              CALL REMSUB ( ERRMSG, 1, L, ERRMSG )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                           END IF

                        END IF

!
!                       End of CK base case.
!
                     END IF

!
!                    End path length case.
!
!                    Now, if the base frame is of CK type,
!                    and if there is no SCLK data for this frame,
!                    generate the SCLK warning message.
!
                     IF ( CLASS .EQ. CK ) THEN

                        CALL CKMETA ( CLSSID, 'SCLK', CLKID )

                        IF (  .NOT.  ZZSCLK( CLSSID, CLKID )  ) THEN

!
!                          Increment the count of bases missing
!                          SCLK data.
!
                           NOSCLK = NOSCLK + 1

                           SCWARN = ' No SCLK kernel for instrument ' &
                           //       'or structure #, with ' &
                           //       'corresponding SCLK ID #, is ' &
                           //       'currently loaded.'

                           CALL REPMI ( SCWARN, '#', CLSSID, SCWARN )
                           CALL REPMI ( SCWARN, '#', CLKID,  SCWARN )

                           L = RTRIM ( SCWARN )

                           IF ( ERRMSG(1:L) .EQ. SCWARN ) THEN

                              CALL REMSUB ( ERRMSG, 1, L, ERRMSG )
                              CALL CHCKXC ( .FALSE., ' ', OK )
                           END IF

                        END IF

                     END IF

                  END DO

!
!                 End of path loop.
!
!                 Create the closing message, if needed.
!
                  IF ( ISCK .AND. ( NOSCLK .LT. 2 )  ) THEN

                     IF ( NOSCLK .EQ. 0 ) THEN

!
!                       We have at least one CK base frame
!                       but no missing SCLK data.
!
                        CLOSNG = ' Failure to find required CK data ' &
                      // 'could be due to one or more CK files ' &
                      // 'not having been loaded, or to the epoch ' &
                      // 'shown above lying within a coverage gap or ' &
                      // 'beyond the coverage bounds of the loaded CK ' &
                      // 'files. It is also possible that no loaded CK ' &
                      // 'file has required angular velocity data for ' &
                      // 'the input epoch, even if a loaded CK ' &
                      // 'does have attitude data for that epoch. ' &
                      // 'You can use CKBRIEF with the -dump option ' &
                      // 'to display coverage intervals of a CK file.'


                     ELSE

!
!                       We're missing one set of SCLK data.
!
                        CLOSNG = ' For a CK frame for which the ' &
                      // 'corresponding SCLK ' &
                      // 'kernel has been loaded, ' &
                      // 'failure to find required CK data ' &
                      // 'could be due to one or more CK files ' &
                      // 'not having been loaded, or to the epoch ' &
                      // 'shown above lying within a coverage gap or ' &
                      // 'beyond the coverage bounds of the loaded CK ' &
                      // 'files. It is also possible that no loaded CK ' &
                      // 'file has required angular velocity data for ' &
                      // 'the input epoch, even if a loaded CK ' &
                      // 'does have attitude data for that epoch. ' &
                      // 'You can use CKBRIEF with the -dump option ' &
                      // 'to display coverage intervals of a CK file.'

                     END IF

!
!                    Remove the closing message.
!
                     L = RTRIM ( CLOSNG )

                     IF ( ERRMSG(1:L) .EQ. CLOSNG ) THEN

                        CALL REMSUB ( ERRMSG, 1, L, ERRMSG )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                     END IF

                  END IF

!
!                 Ok, here's the test. ERRMSG should be blank.
!
                  CALL CHCKSC ( 'ERRMSG post deletion', &
                                ERRMSG, '=', ' ', OK    )

               END IF

!
!              End of J != I case. This is the block in which a
!              test message is created.
!
            END DO
!
!           End of SCLK load loop.
!
         END DO
!
!        End of second path loop.
!
      END DO
!
!     End of first path loop.
!

!
!     Close out the test family.
!

!
!     Clean-up all kernels.
!
      CALL KCLEAR()

      CALL T_SUCCESS ( OK )
      RETURN
      END



