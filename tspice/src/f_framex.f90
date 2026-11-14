!$Procedure      F_FRAMEX ( Family of tests for FRAMEX)

      SUBROUTINE F_FRAMEX ( OK )
      use pepper_legacy, pepper_legacy_frame => frame

! $ Abstract
!
!     This test family checks out the frame expert utility FRAMEX.
!     It exercises all entry points and checks that data loaded
!     in the kernel pool is visible and properly retrieved.
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
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the entry points of the frame system routine
!     FRAMEX.
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
!-    TSPICE Version 1.1.0, 09-FEB-2011 (NJB) (WLT)
!
!        Updated to test CIDFRM bug fix.
!
!        Modified string assignments to suppress gfortran
!        compilation warnings.
!
!        Added header.
!
!
!-&



!
!     Test Utility Functions
!
      CHARACTER*(16)        BEGDAT

!
!     SPICELIB Functions
!


!
!     Local Variables
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 80 )

      CHARACTER*(WDSIZE)    FRNAME
      CHARACTER*(WDSIZE)    NAME
      CHARACTER*(WDSIZE)    ENAME
      CHARACTER*(WDSIZE)    NAMES ( 40 )

      CHARACTER*(TXTSIZ)    TEXT  ( 20 )

      INTEGER               CODE
      INTEGER               CODES ( 40 )
      INTEGER               ECODE
      INTEGER               FRCODE
      INTEGER               CENT
      INTEGER               CLASS
      INTEGER               CLSSID

      LOGICAL               FOUND
      LOGICAL               FOUND1
      LOGICAL               FOUND2

      INTEGER               FRAME

      INTEGER               I
      INTEGER               IDCODE

      INTEGER               SUNIT

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 240 )

      LOGICAL               ISTRUE

      CHARACTER*(LNSIZE)    MESSGE



!
!     Begin every test family with an open call.
!
      CALL TOPEN  ( 'F_FRAMEX' )

      ISTRUE = .TRUE.

      CALL TCASE( 'Perform check to make sure that every ' &
      //          'inertial frame is recognized. ' )
!
!        Note that the upper bound of the loop should be increased
!        if the number of inertial frames increases.
!
         DO I = 1, 18

            FRCODE = I

            CALL FRINFO ( FRCODE, CENT, CLASS, CLSSID, FOUND )

            MESSGE = 'The case failed when the inertial frame ' &
            //       'code supplied to FRINFO was #. '

            CALL TSTMSG ( '#',  MESSGE )
            CALL TSTMSI (       FRCODE )


            CALL CHCKXC ( .FALSE.,  ' ',                OK )
            CALL CHCKSL ( 'FOUND',  FOUND,      ISTRUE, OK )
            CALL CHCKSI ( 'CENTER', CENT,   '=', 0, 0,  OK )
            CALL CHCKSI ( 'CLASS',  CLASS,  '=', 1, 0,  OK )
            CALL CHCKSI ( 'CLSSID', CLSSID, '=', I, 0,  OK )

         END DO

      CALL TCASE ( 'Check to make sure that the recognized ' &
      //           'non-inertial frames are all type PCK. ' )

         DO I = 1, 79

            FRCODE = 10000 + I
            CALL FRINFO ( FRCODE, CENT, CLASS, CLSSID, FOUND )

            MESSGE = 'The case failed when the non-inertial frame ' &
            //       'code supplied to FRINFO was #. '
            CALL TSTMSG ( '#',  MESSGE )
            CALL TSTMSI (       FRCODE )

            CALL CHCKXC ( .FALSE.,  ' ',                OK )
            CALL CHCKSL ( 'FOUND',  FOUND,      ISTRUE, OK )
            CALL CHCKSI ( 'CLASS',  CLASS,  '=', 2, 0,  OK )

         END DO


      CALL TCASE ( 'Make sure that the id-code to name to id-code ' &
      //           'path works.' )

         DO I = 1, 18

            FRCODE = I

            NAME = ' '
            CALL FRMNAM ( FRCODE, NAME )
            CALL NAMFRM ( NAME,   CODE )

            MESSGE = 'The case failed when the frame code was #. '


            CALL TSTMSG ( '#', MESSGE )
            CALL TSTMSI (      FRCODE )

            CALL CHCKXC ( .FALSE.,          ' ',          OK )
            CALL CHCKSI ( 'FRCODE', FRCODE, '=', CODE, 0, OK )
            CALL CHCKSC ( 'NAME', NAME,    '!=', ' ',     OK )


         END DO

         DO I = 1, 79

            FRCODE = 10000 + I

            NAME = ' '
            CALL FRMNAM ( FRCODE, NAME )
            CALL NAMFRM ( NAME,   CODE )

            MESSGE = 'The case failed when the frame code was #. '

            CALL TSTMSG ( '#', MESSGE )
            CALL TSTMSI (      FRCODE )

            CALL CHCKXC ( .FALSE.,          ' ',          OK )
            CALL CHCKSI ( 'FRCODE', FRCODE, '=', CODE, 0, OK )
            CALL CHCKSC ( 'NAME', NAME,    '!=', ' ',     OK )

         END DO

      CALL TCASE ( 'Check to make sure we can get data from the ' &
      //           'kernel pool.')

         CALL KILFIL ( 'framedat.cnk'        )
         CALL TXTOPN ( 'framedat.cnk', SUNIT )

         WRITE (SUNIT,*)  BEGDAT()
         WRITE (SUNIT,*) 'FRAME_20000_CLASS    = 2 '
         WRITE (SUNIT,*) 'FRAME_20000_CENTER   = -399 '
         WRITE (SUNIT,*) 'FRAME_20000_CLASS_ID = -499 '
         WRITE (SUNIT,*) 'FRAME_20000_NAME     = ''TESTFRAME'' '
         WRITE (SUNIT,*) 'FRAME_TESTFRAME      = 20000 '
         WRITE (SUNIT,*) ' '

         CLOSE ( SUNIT )

         MESSGE = 'This is before loading the kernel pool.'

         CALL TSTMSG ( '#', MESSGE )
         CALL FRINFO (  20000, CENT, CLASS, CLSSID, FOUND1 )

         CALL CHCKXC ( .FALSE., ' ',              OK )
         CALL CHCKSL ( 'FOUND1', FOUND1, .FALSE., OK )


         CALL LDPOOL ( 'framedat.cnk' )


         MESSGE = 'This is after loading the kernel pool.'

         CALL TSTMSG ( '#', MESSGE )
         CALL FRMNAM (  20000,      NAME   )
         CALL NAMFRM ( 'TESTFRAME', IDCODE )
         CALL FRINFO (  20000,      CENT, CLASS, CLSSID, FOUND2 )

         CALL CHCKXC ( .FALSE., ' ',                       OK )
         CALL CHCKSL ( 'FOUND2', FOUND2,      ISTRUE,      OK )
         CALL CHCKSC ( 'NAME',   NAME,   '=', 'TESTFRAME', OK )
         CALL CHCKSI ( 'IDCODE', IDCODE, '=',  20000, 0,   OK )
         CALL CHCKSI ( 'CENT',   CENT,   '=',   -399, 0,   OK )
         CALL CHCKSI ( 'CLASS',  CLASS,  '=',      2, 0,   OK )
         CALL CHCKSI ( 'CLSSID', CLSSID, '=',   -499, 0,   OK )

         CALL KILFIL ( 'framedat.cnk' )
         CALL TSTMSG ( '#', ' ' )


      CALL TCASE ( 'Make sure that unrecognized names don''t ' &
      //           'produce a non-zero frame code. ' )

         CALL NAMFRM ( 'SPUD', IDCODE )

         CALL CHCKXC ( .FALSE., ' ',                  OK )
         CALL CHCKSI ( 'IDCODE', IDCODE, '=', 0, 0,   OK )

      CALL TCASE ( 'Make sure that unknown idcodes produce blank ' &
      //           'names and that they are not found by FRINFO ' )

         CALL FRMNAM ( 1000000, NAME )
         CALL FRINFO ( 1000000, CENT, CLASS, CLSSID, FOUND )

         CALL CHCKXC ( .FALSE., ' ',                  OK )
         CALL CHCKSL ( 'FOUND2', FOUND,      .FALSE., OK )
         CALL CHCKSC ( 'NAME',   NAME,   '=', ' ',    OK )


      CALL TCASE ( 'Make sure that frames with id-codes in the ' &
      //           'range from 13001 to 13999 produce correct ' &
      //           'CENT, CLASS, and CLSSID values. ' )


         DO I = 13001, 13999, 37

            FRAME = I
            CALL FRINFO ( FRAME, CENT, CLASS, CLSSID, FOUND )

            CALL CHCKXC ( .FALSE., ' ',                        OK )
            CALL CHCKSL ( 'FOUND', FOUND,     .TRUE.,          OK )
            CALL CHCKSI ( 'CENT',  CENT,  '=', 399,         0, OK )
            CALL CHCKSI ( 'CENT',  CLASS, '=', 2,           0, OK )
            CALL CHCKSI ( 'CENT',  CLSSID,'=', FRAME-10000, 0, OK )

         END DO

      CALL TCASE ( 'Make sure the frame ''ITRF93'' is a ' &
      //           'recognized frame and the frame information ' &
      //           'associated with it is correct. ' )

         CALL NAMFRM ( 'ITRF93', FRAME )
         CALL FRMNAM (  FRAME,   NAME  )
         CALL FRINFO (  FRAME,   CENT, CLASS, CLSSID, FOUND )

         CALL CHCKXC ( .FALSE., ' ',                        OK )

         CALL CHCKSL ( 'FOUND', FOUND,     .TRUE.,          OK )
         CALL CHCKSI ( 'CENT',  CENT,   '=', 399,         0, OK )
         CALL CHCKSI ( 'CENT',  CLASS,  '=', 2,           0, OK )
         CALL CHCKSI ( 'CENT',  CLSSID, '=', 3000,        0, OK )
         CALL CHCKSC ( 'NAME',  NAME,   '=','ITRF93',        OK )
         CALL CHCKSI ( 'FRAME', FRAME,  '=', 13000,       0, OK )

      CALL TCASE ( 'Make sure the frame ''EARTH_FIXED'' is a ' &
      //           'recognized frame and the frame information ' &
      //           'associated with it is correct. ' )

         CALL NAMFRM ( 'EARTH_FIXED', FRAME )
         CALL FRMNAM (  FRAME,        NAME  )
         CALL FRINFO (  FRAME,        CENT, CLASS, CLSSID, FOUND )

         CALL CHCKXC ( .FALSE., ' ',                        OK )

         CALL CHCKSL ( 'FOUND', FOUND,     .TRUE.,          OK )
         CALL CHCKSI ( 'CENT',  CENT,   '=', 399,         0, OK )
         CALL CHCKSI ( 'CENT',  CLASS,  '=', 4,           0, OK )
         CALL CHCKSI ( 'CENT',  CLSSID, '=', 10081,       0, OK )
         CALL CHCKSC ( 'NAME',  NAME,   '=','EARTH_FIXED',   OK )
         CALL CHCKSI ( 'FRAME', FRAME,  '=', 10081,       0, OK )


      CALL TCASE ( 'Make sure that the frame to associate with ' &
      //           'the planets and satellites are known from ' &
      //           'the built-in list. ' )



         NAMES (  1 ) = 'MERCURY'
         NAMES (  2 ) = 'VENUS'
         NAMES (  3 ) = 'EARTH'
         NAMES (  4 ) = 'MARS'
         NAMES (  5 ) = 'JUPITER'
         NAMES (  6 ) = 'SATURN'
         NAMES (  7 ) = 'URANUS'
         NAMES (  8 ) = 'NEPTUNE'
         NAMES (  9 ) = 'PLUTO'
         NAMES ( 10 ) = 'SUN'
         NAMES ( 11 ) = 'MOON'
         NAMES ( 12 ) = 'PHOBOS'
         NAMES ( 13 ) = 'DEIMOS'
         NAMES ( 14 ) = 'IO'
         NAMES ( 15 ) = 'EUROPA'
         NAMES ( 16 ) = 'GANYMEDE'
         NAMES ( 17 ) = 'CALLISTO'
         NAMES ( 18 ) = 'AMALTHEA'
         NAMES ( 19 ) = 'ADRASTEA'
         NAMES ( 20 ) = 'METIS'
         NAMES ( 21 ) = 'TITAN'
         NAMES ( 22 ) = 'OBERON'
         NAMES ( 23 ) = 'TITANIA'
         NAMES ( 24 ) = 'UMBRIEL'
         NAMES ( 25 ) = 'PUCK'
         NAMES ( 26 ) = 'MIRANDA'
         NAMES ( 27 ) = 'HIMALIA'
         NAMES ( 28 ) = 'ARIEL'
         NAMES ( 29 ) = 'ELARA'
         NAMES ( 30 ) = 'TRITON'
         NAMES ( 31 ) = 'NEREID'
         NAMES ( 32 ) = 'CHARON'

         DO I = 1, 32

            CALL CNMFRM ( NAMES(I), FRCODE, FRNAME, FOUND )

            ENAME = 'IAU_'//NAMES(I)(:WDSIZE-4)

            CALL CHCKXC ( .FALSE., ' ',            OK )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
            CALL CHCKSC ( 'FRNAME', FRNAME, '=', ENAME, OK )

            CALL NAMFRM (  FRNAME,  ECODE                     )
            CALL CHCKSI ( 'FRCODE', FRCODE, '=', ECODE, 0, OK )

         END DO


      CALL TCASE ( 'Verify that the mapping between centers ' &
      //           'input as id-codes and associated frames is ' &
      //           'correctly maintained for the default set of ' &
      //           'bodyfixed frames. ' )



         CODES(  1 ) = 199
         CODES(  2 ) = 299
         CODES(  3 ) = 399
         CODES(  4 ) = 499
         CODES(  5 ) = 599
         CODES(  6 ) = 699
         CODES(  7 ) = 799
         CODES(  8 ) = 899
         CODES(  9 ) = 999
         CODES( 10 ) = 301
         CODES( 11 ) = 10
         CODES( 12 ) = 401
         CODES( 13 ) = 402
         CODES( 14 ) = 501
         CODES( 15 ) = 502
         CODES( 16 ) = 503
         CODES( 17 ) = 504
         CODES( 18 ) = 505
         CODES( 19 ) = 506
         CODES( 20 ) = 507
         CODES( 21 ) = 508
         CODES( 22 ) = 509
         CODES( 23 ) = 510
         CODES( 24 ) = 601
         CODES( 25 ) = 602
         CODES( 26 ) = 603
         CODES( 27 ) = 604
         CODES( 28 ) = 605
         CODES( 29 ) = 606
         CODES( 30 ) = 607
         CODES( 31 ) = 701
         CODES( 32 ) = 702
         CODES( 33 ) = 703
         CODES( 34 ) = 704
         CODES( 35 ) = 705
         CODES( 36 ) = 801
         CODES( 37 ) = 802
         CODES( 38 ) = 901
         CODES( 39 ) = 801

         DO I = 1,39

            CALL BODC2N ( CODES(I), NAME, FOUND )

            ENAME = 'IAU_' // NAME(:WDSIZE-4)

            CALL CIDFRM ( CODES(I), FRCODE, FRNAME, FOUND )

            CALL CHCKXC ( .FALSE., ' ',            OK )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
            CALL CHCKSC ( 'FRNAME', FRNAME, '=', ENAME, OK )

            CALL NAMFRM (  FRNAME,  ECODE                     )
            CALL CHCKSI ( 'FRCODE', FRCODE, '=', ECODE, 0, OK )

            CALL FRINFO ( FRCODE,   CODE, CLASS, CLSSID, FOUND )

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,          OK )
            CALL CHCKSI ( 'CODE',  CODE, '=', CODES(I), 0, OK )


         END DO


      CALL TCASE ( 'Load a preferred frame into the kernel pool ' &
      //           'and make sure that the frame subsystem can ' &
      //           'locate the preferred frame. ' )


         TEXT(  1 ) = BEGDAT()
         TEXT(  2 ) = 'OBJECT_EARTH_FRAME = ''ITRF93'''
         TEXT(  3 ) = 'OBJECT_199_FRAME   = ''J2000'''
         TEXT(  4 ) = 'FRAME_EROSFIXED        = 1000001'
         TEXT(  5 ) = 'FRAME_1000001_NAME     = ''EROSFIXED'''
         TEXT(  6 ) = 'FRAME_1000001_CLASS    = 2'
         TEXT(  7 ) = 'FRAME_1000001_CLASS_ID = 2000433'
         TEXT(  8 ) = 'FRAME_1000001_CENTER   = 2000433'
         TEXT(  9 ) = ' '
         TEXT( 10 ) = 'OBJECT_2000433_FRAME   = ''EROSFIXED'''


         CALL KILFIL ( 'framedat.cnk'        )
         CALL TSTTXT ( 'framedat.cnk', TEXT, 10, .TRUE., .FALSE.   )

!
!        Check CIDFRM and CNMFRM to make sure they can find
!        a built-in frame associated with the Earth via a
!        kernel pool assignment.
!
         CALL CIDFRM ( 399, FRCODE, FRNAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'ITRF93',  OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',  13000, 0, OK )


         CALL CNMFRM ( 'EARTH', FRCODE, FRNAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'ITRF93',  OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',  13000, 0, OK )

         CALL CIDFRM ( 199, FRCODE, FRNAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'J2000',   OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',      1, 0, OK )

         CALL CNMFRM ( 'MERCURY', FRCODE, FRNAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'J2000',  OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',     1, 0, OK )

         CALL CNMFRM ( 'MATHILDE', FRCODE, FRNAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ',             OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )

         CALL CIDFRM ( 1000131, FRCODE, FRNAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ',             OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )


!
!        Check CIDFRM and CNMFRM to make sure they can find
!        a TK frame associated with Eros via a
!        kernel pool assignment.
!
         CALL CIDFRM ( 2000433, FRCODE, FRNAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'EROSFIXED',  OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',  1000001,  0, OK )


         CALL CNMFRM ( 'EROS', FRCODE, FRNAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'EROSFIXED',  OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',  1000001,  0, OK )


      CALL TCASE ( 'Repeat previous test; identify RHS frames ' &
      //           'in object-frame assignment via frame ID code.' )


         CALL CLPOOL()


         TEXT(  1 ) = BEGDAT()
         TEXT(  2 ) = 'OBJECT_EARTH_FRAME =  13000'
         TEXT(  3 ) = 'OBJECT_199_FRAME   =  1'
         TEXT(  4 ) = 'FRAME_EROSFIXED        = 1000001'
         TEXT(  5 ) = 'FRAME_1000001_NAME     = ''EROSFIXED'''
         TEXT(  6 ) = 'FRAME_1000001_CLASS    = 2'
         TEXT(  7 ) = 'FRAME_1000001_CLASS_ID = 2000433'
         TEXT(  8 ) = 'FRAME_1000001_CENTER   = 2000433'
         TEXT(  9 ) = ' '
         TEXT( 10 ) = 'OBJECT_2000433_FRAME   = ''EROSFIXED'''


         CALL KILFIL ( 'framedat.cnk'        )
         CALL TSTTXT ( 'framedat.cnk', TEXT, 10, .TRUE., .FALSE.   )

!
!        Check CIDFRM and CNMFRM to make sure they can find
!        a built-in frame associated with the Earth via a
!        kernel pool assignment.
!
         CALL CIDFRM ( 399, FRCODE, FRNAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'ITRF93',  OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',  13000, 0, OK )


         CALL CNMFRM ( 'EARTH', FRCODE, FRNAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'ITRF93',  OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',  13000, 0, OK )

         CALL CIDFRM ( 199, FRCODE, FRNAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'J2000',   OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',      1, 0, OK )

         CALL CNMFRM ( 'MERCURY', FRCODE, FRNAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE., OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'J2000',  OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',     1, 0, OK )

         CALL CNMFRM ( 'MATHILDE', FRCODE, FRNAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ',             OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )

         CALL CIDFRM ( 1000131, FRCODE, FRNAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ',             OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .FALSE., OK )


!
!        Check CIDFRM and CNMFRM to make sure they can find
!        a TK frame associated with Eros via a
!        kernel pool assignment.
!
         CALL CIDFRM ( 2000433, FRCODE, FRNAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'EROSFIXED',  OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',  1000001,  0, OK )


         CALL CNMFRM ( 'EROS', FRCODE, FRNAME, FOUND )
         CALL CHCKXC ( .FALSE., ' ',            OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'EROSFIXED',  OK )
         CALL CHCKSI ( 'FRCODE', FRCODE, '=',  1000001,  0, OK )




      CALL T_SUCCESS( OK )
      RETURN
      END
