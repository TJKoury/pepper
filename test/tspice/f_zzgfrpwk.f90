!$Procedure      F_ZZGFRPWK ( Test ZZGFRPWK entry points )

      SUBROUTINE F_ZZGFRPWK ( OK )

! $ Abstract
!
!     Test the GF progress reporting entry points contained in ZZGFRPWK.
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
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'zzgf.inc'

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
!     This family tests the GF progress reporting routines
!
!        GFREPI
!        GFREPU
!        GFREPF
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 20-NOV-2021 (NJB)
!
!        Increased work done in test case that creates multiple
!        status lines.
!
!        Updated comments and changed variable name for wait time.
!        Deleted declaration of unused parameter LBCELL.
!
!-    SPICELIB Version 1.1.0, 30-MAR-2009 (BVS)
!
!        Fixed arguments in the ZZGFRPWK call.
!
!-    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB)
!
!-&


!
!     SPICELIB functions
!

      LOGICAL               EXISTS

!
!     Local parameters
!
      CHARACTER*(*)         REPFIL
      PARAMETER           ( REPFIL = 'zzgfrpwk.txt' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 400 )

!
!     Local Variables
!
      CHARACTER*(MXBEGM)    BEGMSG
      CHARACTER*(MXENDM)    ENDMSG
      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(LNSIZE)    PCTSTR
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(MXBEGM)    XBEGMS
      CHARACTER*(MXENDM)    XENDMS
      CHARACTER*(LNSIZE)    XLINE

      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      INCR
      DOUBLE PRECISION      TOTAL
      DOUBLE PRECISION      WAIT
      DOUBLE PRECISION      XINCR
      DOUBLE PRECISION      XTOTAL
      DOUBLE PRECISION      XWAIT

      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               NRPORT
      INTEGER               TCHECK
      INTEGER               UNIT
      INTEGER               XCHECK
      INTEGER               XNL
      INTEGER               XUNIT

!
!     Saved everything
!
      SAVE

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFRPWK' )






!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test ZZGFTSWK parameter storage.' )


!
!     Open a text file to capture the progress report.
!
      IF ( EXISTS(REPFIL) ) THEN
         CALL DELFIL( REPFIL )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF


      CALL TXTOPN ( REPFIL, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFWKUN ( XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      XTOTAL = MAXWIN / 2.D0

      XBEGMS = '@-------------------Message prefix--------------------@'
      XENDMS = '@***suffix**@'

      XWAIT  = 2.D0
      XCHECK = 10

!
!     Call the progress report display initializer.
!
      CALL ZZGFTSWK ( XTOTAL, XWAIT, XCHECK, XBEGMS, XENDMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fetch the current report parameters from the ZZGFRPWK monitor.
!
      CALL ZZGFWKMO ( UNIT, TOTAL, WAIT, TCHECK, BEGMSG, ENDMSG, INCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the stored values from ZZGFWKMO against the expected
!     values.

      CALL CHCKSI ( 'UNIT',   UNIT,   '=', XUNIT,  0,      OK )
      CALL CHCKSD ( 'TOTAL',  TOTAL,  '~', XTOTAL, VTIGHT, OK )
      CALL CHCKSD ( 'WAIT',   WAIT,   '=', XWAIT,  0.D0,   OK )
      CALL CHCKSI ( 'TCHECK', TCHECK, '=', XCHECK, 0,      OK )
      CALL CHCKSC ( 'BEGMSG', BEGMSG, '=', XBEGMS,         OK )
      CALL CHCKSC ( 'ENDMSG', ENDMSG, '=', XENDMS,         OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test the report file created by a ' &
      //           'call to ZZGFTSWK'                  )


!
!     Close and delete the existing log.
!
      CLOSE ( XUNIT )
      CALL DELFIL ( REPFIL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open a new log file.
!
      CALL TXTOPN ( REPFIL, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFWKUN ( XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set up a search.
!
      XTOTAL = MAXWIN / 2.D0

      XBEGMS = '@-------------------Message prefix--------------------@'
      XENDMS = '@***suffix**@'

      XWAIT  = 2.D0
      XCHECK = 10

!
!     Call the progress report display initializer.
!
      CALL ZZGFTSWK ( XTOTAL, XWAIT, XCHECK, XBEGMS, XENDMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Rewind the file to set the record pointer to the start
!     of the file.
!
      REWIND ( XUNIT, IOSTAT = IOSTAT )
      CALL CHCKSI ( 'REWIND IOSTAT', IOSTAT, '=', 0,  0,  OK )

      IF ( IOSTAT .EQ. 0 ) THEN
!
!        The first two lines of the file should be blank.
!
         DO I = 1, 2

            QNAME = 'Read IOSTAT no. *'
            CALL REPMI ( QNAME, '*', I, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            READ ( XUNIT, '(A)', IOSTAT = IOSTAT ) LINE
            CALL CHCKSI ( QNAME, IOSTAT, '=', 0,  0,  OK )

            IF ( IOSTAT .EQ. 0 ) THEN

               QNAME = 'Log file line no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSC ( QNAME, LINE, '=', ' ', OK )

            END IF

         END DO

      END IF

!
!     If we haven't hit an error, read the third line.
!
      IF ( IOSTAT .EQ. 0 ) THEN

         READ ( XUNIT, '(A)', IOSTAT = IOSTAT ) LINE
         CALL CHCKSI ( QNAME, IOSTAT, '=', 0,  0,  OK )

         IF ( IOSTAT .EQ. 0 ) THEN
!
!           Check the report line.
!
            XLINE = XBEGMS
            CALL SUFFIX ( '  0.00%', 1, XLINE )
            CALL SUFFIX ( XENDMS,    1, XLINE )

            CALL CHCKSC ( 'LINE', LINE, '=', XLINE, OK )

         END IF

      END IF



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test ZZGFKIN parameter storage.' )


      DO I = 1, 5

         XINCR = DBLE(I)

         CALL ZZGFWKIN ( XINCR )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZGFWKMO ( UNIT,   TOTAL,  WAIT, &
                         TCHECK, BEGMSG, ENDMSG, INCR )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        We expect the last work increment to be I.
!

         QNAME = 'INCR I=1'

         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( QNAME, INCR, '~', XINCR, VTIGHT, OK )

      END DO


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test ZZGFWKAD parameter storage.'   )

      XWAIT  = 17.D0
      XCHECK = 32
      XBEGMS = '$-------Message prefix--------------------------------$'
      XENDMS = '#***suffix**#'

      CALL ZZGFWKAD ( XWAIT, XCHECK, XBEGMS, XENDMS  )
      CALL CHCKXC ( .FALSE., ' ', OK )


!     Fetch the current report parameters from the ZZGFRPWK monitor.
!
      CALL ZZGFWKMO ( UNIT, TOTAL, WAIT, TCHECK, BEGMSG, ENDMSG, INCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the stored values from ZZGFWKMO against the expected
!     values.
!
!     GFREPF is supposed to set the poll duration and call count to 0
!     seconds and 1 call, respectively. The work increment should be
!     set to 0.D0. These values are hard-coded in GFREPF.
!
      CALL CHCKSI ( 'UNIT',   UNIT,   '=', XUNIT,  0,      OK )
      CALL CHCKSD ( 'TOTAL',  TOTAL,  '~', XTOTAL, VTIGHT, OK )
      CALL CHCKSD ( 'WAIT',   WAIT,   '=', XWAIT,  0.D0,   OK )
      CALL CHCKSI ( 'TCHECK', TCHECK, '=', XCHECK, 0,      OK )
      CALL CHCKSC ( 'BEGMSG', BEGMSG, '=', XBEGMS,         OK )
      CALL CHCKSC ( 'ENDMSG', ENDMSG, '=', XENDMS,         OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test a report with multiple status lines.' )


!
!     Close and delete the existing log.
!
      CLOSE ( XUNIT )
      CALL DELFIL ( REPFIL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open a new log file.
!
      CALL TXTOPN ( REPFIL, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFWKUN ( XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set up a search.
!
      XTOTAL = 100.D0

      XBEGMS = '@-----------------------------------Message prefix----@'
      XENDMS = '@*****suffix@'

!
!     Check the system time every 10 increment calls.
!
      XCHECK = 10

!
!     Set the wait time for display to zero.
!
      XWAIT = 0.D0

!
!     Call the progress report display initializer.
!
      CALL ZZGFTSWK ( XTOTAL, XWAIT, XCHECK, XBEGMS, XENDMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Report status 100 times. Each time, the increment is "1".
!
      DO I = 1, 100

         CALL ZZGFWKIN ( 1.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Rewind the file to set the record pointer to the start
!     of the file.
!
      REWIND ( XUNIT, IOSTAT = IOSTAT )
      CALL CHCKSI ( 'REWIND IOSTAT', IOSTAT, '=', 0,  0,  OK )

      IF ( IOSTAT .EQ. 0 ) THEN
!
!        The first two lines of the file should be blank.
!
         DO I = 1, 2

            QNAME = 'Read IOSTAT no. *'
            CALL REPMI ( QNAME, '*', I, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            READ ( XUNIT, '(A)', IOSTAT = IOSTAT ) LINE
            CALL CHCKSI ( QNAME, IOSTAT, '=', 0,  0,  OK )

            IF ( IOSTAT .EQ. 0 ) THEN

               QNAME = 'Log file line no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSC ( QNAME, LINE, '=', ' ', OK )

            END IF

         END DO

      END IF

!
!     If we haven't hit an error, read the rest of the file.
!
      IF ( IOSTAT .EQ. 0 ) THEN

         DO I = 1, NINT( XTOTAL / XCHECK ) + 1

            QNAME = 'Log file line no. * IOSTAT'
            CALL REPMI ( QNAME, '*', I+2, QNAME )

            READ ( XUNIT, '(A)', IOSTAT = IOSTAT ) LINE
            CALL CHCKSI ( QNAME, IOSTAT, '=', 0,  0,  OK )

            IF ( IOSTAT .EQ. 0 ) THEN
!
!              Check the report line.
!
               CALL DPFMT ( DBLE(10*(I-1)), 'XXX.XX', PCTSTR )

               XLINE = XBEGMS
               CALL SUFFIX ( PCTSTR, 1, XLINE )
               CALL SUFFIX ( '%',    0, XLINE )
               CALL SUFFIX ( XENDMS, 1, XLINE )

               CALL CHCKSC ( 'LINE', LINE, '=', XLINE, OK )

            END IF

         END DO

      END IF






!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test a report with multiple status lines, ' &
      //           'this time using a positive time delay.'   )


!
!     Close and delete the existing log.
!
      CLOSE ( XUNIT )
      CALL DELFIL ( REPFIL )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open a new log file.
!
      CALL TXTOPN ( REPFIL, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFWKUN ( XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set up a search. The search must take long enough
!     so that at least one XWAIT seconds pass per 20% of the
!     work.
!
      XTOTAL = 2.5D8

      XBEGMS = '@-Message prefix--------------------------------------@'
      XENDMS = '@****Suffix*@'

!
!     Check the system time once every XTOTAL/20 increment calls.
!
      XCHECK = NINT( XTOTAL / 20 )

!
!     Set the wait time for display in seconds. The wait time is a
!     lower bound on the duration between progress report display
!     updates. Setting the wait to a small value won't cause more
!     updates to be displayed; in no event will there be more updates
!     than allowed by XCHECK. Setting the wait to a large value will
!     cause the test case to fail because some progress updates won't
!     get displayed.
!
      XWAIT = 1.D-1

!
!     Call the progress report display initializer.
!
      CALL ZZGFTSWK ( XTOTAL, XWAIT, XCHECK, XBEGMS, XENDMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the progress increment to 5.
!
      INCR = 5

!
!     Report status XTOTAL/INCR times. So every time the
!     system clock is checked, we expect that
!
!          FRAC = XCHECK * INCR / XTOTAL
!
!
!               = 25%
!
!     progress should have been made since the last check. We expect
!     25% of the computation to take longer than XWAIT seconds, so we
!     should see a report line for every 25% progress.
!
      FRAC   = XCHECK * INCR / XTOTAL

      NRPORT = NINT( XTOTAL / INCR )


      DO I = 1, NRPORT

         CALL ZZGFWKIN ( INCR )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     Rewind the file to set the record pointer to the start
!     of the file.
!
      REWIND ( XUNIT, IOSTAT = IOSTAT )
      CALL CHCKSI ( 'REWIND IOSTAT', IOSTAT, '=', 0,  0,  OK )

      IF ( IOSTAT .EQ. 0 ) THEN
!
!        The first two lines of the file should be blank.
!
         DO I = 1, 2

            QNAME = 'Read IOSTAT no. *'
            CALL REPMI ( QNAME, '*', I, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            READ ( XUNIT, '(A)', IOSTAT = IOSTAT ) LINE
            CALL CHCKSI ( QNAME, IOSTAT, '=', 0,  0,  OK )

            IF ( IOSTAT .EQ. 0 ) THEN

               QNAME = 'Log file line no. *'
               CALL REPMI ( QNAME, '*', I, QNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSC ( QNAME, LINE, '=', ' ', OK )

            END IF

         END DO

      END IF

!
!     If we haven't hit an error, read the rest of the file.
!
!     The expected number of status lines in the report is the
!     number of update calls divided by the call check ratio,
!     plus one, since the first line shows 0% progress.
!
      XNL = 1 + ( NRPORT / XCHECK )


      IF ( IOSTAT .EQ. 0 ) THEN

         DO I = 1, XNL

            QNAME = 'Log file line no. * IOSTAT'
            CALL REPMI ( QNAME, '*', I+2, QNAME )

            READ ( XUNIT, '(A)', IOSTAT = IOSTAT ) LINE
            CALL CHCKSI ( QNAME, IOSTAT, '=', 0,  0,  OK )


            IF ( IOSTAT .EQ. 0 ) THEN
!
!              Check the report line.
!
               CALL DPFMT ( (I-1)*100*FRAC, 'XXX.XX', PCTSTR )

               XLINE = XBEGMS
               CALL SUFFIX ( PCTSTR, 1, XLINE )
               CALL SUFFIX ( '%',    0, XLINE )
               CALL SUFFIX ( XENDMS, 1, XLINE )

               CALL CHCKSC ( 'LINE', LINE, '=', XLINE, OK )

            END IF

         END DO

      END IF


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Clean up: delete log file.' )

      IF ( EXISTS(REPFIL) ) THEN

         CLOSE ( XUNIT )

         CALL DELFIL( REPFIL )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Call to umbrella routine' )

      CALL ZZGFRPWK ( 1, 2.D0, 3.D0, 7, ' ', ' ', 0.5D0 )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )




      CALL T_SUCCESS ( OK )
      RETURN
      END
