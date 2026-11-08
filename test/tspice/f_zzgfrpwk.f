C$Procedure      F_ZZGFRPWK ( Test ZZGFRPWK entry points )

      SUBROUTINE F_ZZGFRPWK ( OK )

C$ Abstract
C
C     Test the GF progress reporting entry points contained in ZZGFRPWK.
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
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'zzgf.inc'

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
C     None.
C
C$ Particulars
C
C     This family tests the GF progress reporting routines
C
C        GFREPI
C        GFREPU
C        GFREPF
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 20-NOV-2021 (NJB)
C
C        Increased work done in test case that creates multiple
C        status lines.
C
C        Updated comments and changed variable name for wait time.
C        Deleted declaration of unused parameter LBCELL.
C
C-    SPICELIB Version 1.1.0, 30-MAR-2009 (BVS)
C
C        Fixed arguments in the ZZGFRPWK call.
C
C-    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB)
C
C-&


C
C     SPICELIB functions
C

      LOGICAL               EXISTS

C
C     Local parameters
C
      CHARACTER*(*)         REPFIL
      PARAMETER           ( REPFIL = 'zzgfrpwk.txt' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 400 )

C
C     Local Variables
C
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

C
C     Saved everything
C
      SAVE

C
C     Initial values
C


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFRPWK' )






C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test ZZGFTSWK parameter storage.' )


C
C     Open a text file to capture the progress report.
C
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

C
C     Call the progress report display initializer.
C
      CALL ZZGFTSWK ( XTOTAL, XWAIT, XCHECK, XBEGMS, XENDMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fetch the current report parameters from the ZZGFRPWK monitor.
C
      CALL ZZGFWKMO ( UNIT, TOTAL, WAIT, TCHECK, BEGMSG, ENDMSG, INCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the stored values from ZZGFWKMO against the expected
C     values.

      CALL CHCKSI ( 'UNIT',   UNIT,   '=', XUNIT,  0,      OK )
      CALL CHCKSD ( 'TOTAL',  TOTAL,  '~', XTOTAL, VTIGHT, OK )
      CALL CHCKSD ( 'WAIT',   WAIT,   '=', XWAIT,  0.D0,   OK )
      CALL CHCKSI ( 'TCHECK', TCHECK, '=', XCHECK, 0,      OK )
      CALL CHCKSC ( 'BEGMSG', BEGMSG, '=', XBEGMS,         OK )
      CALL CHCKSC ( 'ENDMSG', ENDMSG, '=', XENDMS,         OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test the report file created by a '
     .//           'call to ZZGFTSWK'                  )


C
C     Close and delete the existing log.
C
      CLOSE ( XUNIT )
      CALL DELFIL ( REPFIL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Open a new log file.
C
      CALL TXTOPN ( REPFIL, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFWKUN ( XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set up a search.
C
      XTOTAL = MAXWIN / 2.D0

      XBEGMS = '@-------------------Message prefix--------------------@'
      XENDMS = '@***suffix**@'

      XWAIT  = 2.D0
      XCHECK = 10

C
C     Call the progress report display initializer.
C
      CALL ZZGFTSWK ( XTOTAL, XWAIT, XCHECK, XBEGMS, XENDMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Rewind the file to set the record pointer to the start
C     of the file.
C
      REWIND ( XUNIT, IOSTAT = IOSTAT )
      CALL CHCKSI ( 'REWIND IOSTAT', IOSTAT, '=', 0,  0,  OK )

      IF ( IOSTAT .EQ. 0 ) THEN
C
C        The first two lines of the file should be blank.
C
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

C
C     If we haven't hit an error, read the third line.
C
      IF ( IOSTAT .EQ. 0 ) THEN

         READ ( XUNIT, '(A)', IOSTAT = IOSTAT ) LINE
         CALL CHCKSI ( QNAME, IOSTAT, '=', 0,  0,  OK )

         IF ( IOSTAT .EQ. 0 ) THEN
C
C           Check the report line.
C
            XLINE = XBEGMS
            CALL SUFFIX ( '  0.00%', 1, XLINE )
            CALL SUFFIX ( XENDMS,    1, XLINE )

            CALL CHCKSC ( 'LINE', LINE, '=', XLINE, OK )

         END IF

      END IF



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test ZZGFKIN parameter storage.' )


      DO I = 1, 5

         XINCR = DBLE(I)

         CALL ZZGFWKIN ( XINCR )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZGFWKMO ( UNIT,   TOTAL,  WAIT,
     .                   TCHECK, BEGMSG, ENDMSG, INCR )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        We expect the last work increment to be I.
C

         QNAME = 'INCR I=1'

         CALL REPMI ( QNAME, '*', I, QNAME )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( QNAME, INCR, '~', XINCR, VTIGHT, OK )

      END DO


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test ZZGFWKAD parameter storage.'   )

      XWAIT  = 17.D0
      XCHECK = 32
      XBEGMS = '$-------Message prefix--------------------------------$'
      XENDMS = '#***suffix**#'

      CALL ZZGFWKAD ( XWAIT, XCHECK, XBEGMS, XENDMS  )
      CALL CHCKXC ( .FALSE., ' ', OK )


C     Fetch the current report parameters from the ZZGFRPWK monitor.
C
      CALL ZZGFWKMO ( UNIT, TOTAL, WAIT, TCHECK, BEGMSG, ENDMSG, INCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the stored values from ZZGFWKMO against the expected
C     values.
C
C     GFREPF is supposed to set the poll duration and call count to 0
C     seconds and 1 call, respectively. The work increment should be
C     set to 0.D0. These values are hard-coded in GFREPF.
C
      CALL CHCKSI ( 'UNIT',   UNIT,   '=', XUNIT,  0,      OK )
      CALL CHCKSD ( 'TOTAL',  TOTAL,  '~', XTOTAL, VTIGHT, OK )
      CALL CHCKSD ( 'WAIT',   WAIT,   '=', XWAIT,  0.D0,   OK )
      CALL CHCKSI ( 'TCHECK', TCHECK, '=', XCHECK, 0,      OK )
      CALL CHCKSC ( 'BEGMSG', BEGMSG, '=', XBEGMS,         OK )
      CALL CHCKSC ( 'ENDMSG', ENDMSG, '=', XENDMS,         OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test a report with multiple status lines.' )


C
C     Close and delete the existing log.
C
      CLOSE ( XUNIT )
      CALL DELFIL ( REPFIL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Open a new log file.
C
      CALL TXTOPN ( REPFIL, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFWKUN ( XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set up a search.
C
      XTOTAL = 100.D0

      XBEGMS = '@-----------------------------------Message prefix----@'
      XENDMS = '@*****suffix@'

C
C     Check the system time every 10 increment calls.
C
      XCHECK = 10

C
C     Set the wait time for display to zero.
C
      XWAIT = 0.D0

C
C     Call the progress report display initializer.
C
      CALL ZZGFTSWK ( XTOTAL, XWAIT, XCHECK, XBEGMS, XENDMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Report status 100 times. Each time, the increment is "1".
C
      DO I = 1, 100

         CALL ZZGFWKIN ( 1.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Rewind the file to set the record pointer to the start
C     of the file.
C
      REWIND ( XUNIT, IOSTAT = IOSTAT )
      CALL CHCKSI ( 'REWIND IOSTAT', IOSTAT, '=', 0,  0,  OK )

      IF ( IOSTAT .EQ. 0 ) THEN
C
C        The first two lines of the file should be blank.
C
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

C
C     If we haven't hit an error, read the rest of the file.
C
      IF ( IOSTAT .EQ. 0 ) THEN

         DO I = 1, NINT( XTOTAL / XCHECK ) + 1

            QNAME = 'Log file line no. * IOSTAT'
            CALL REPMI ( QNAME, '*', I+2, QNAME )

            READ ( XUNIT, '(A)', IOSTAT = IOSTAT ) LINE
            CALL CHCKSI ( QNAME, IOSTAT, '=', 0,  0,  OK )

            IF ( IOSTAT .EQ. 0 ) THEN
C
C              Check the report line.
C
               CALL DPFMT ( DBLE(10*(I-1)), 'XXX.XX', PCTSTR )

               XLINE = XBEGMS
               CALL SUFFIX ( PCTSTR, 1, XLINE )
               CALL SUFFIX ( '%',    0, XLINE )
               CALL SUFFIX ( XENDMS, 1, XLINE )

               CALL CHCKSC ( 'LINE', LINE, '=', XLINE, OK )

            END IF

         END DO

      END IF






C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test a report with multiple status lines, '
     .//           'this time using a positive time delay.'   )


C
C     Close and delete the existing log.
C
      CLOSE ( XUNIT )
      CALL DELFIL ( REPFIL )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Open a new log file.
C
      CALL TXTOPN ( REPFIL, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZGFWKUN ( XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set up a search. The search must take long enough
C     so that at least one XWAIT seconds pass per 20% of the
C     work.
C
      XTOTAL = 2.5D8

      XBEGMS = '@-Message prefix--------------------------------------@'
      XENDMS = '@****Suffix*@'

C
C     Check the system time once every XTOTAL/20 increment calls.
C
      XCHECK = NINT( XTOTAL / 20 )

C
C     Set the wait time for display in seconds. The wait time is a
C     lower bound on the duration between progress report display
C     updates. Setting the wait to a small value won't cause more
C     updates to be displayed; in no event will there be more updates
C     than allowed by XCHECK. Setting the wait to a large value will
C     cause the test case to fail because some progress updates won't
C     get displayed.
C
      XWAIT = 1.D-1

C
C     Call the progress report display initializer.
C
      CALL ZZGFTSWK ( XTOTAL, XWAIT, XCHECK, XBEGMS, XENDMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the progress increment to 5.
C
      INCR = 5

C
C     Report status XTOTAL/INCR times. So every time the
C     system clock is checked, we expect that
C
C          FRAC = XCHECK * INCR / XTOTAL
C
C
C               = 25%
C
C     progress should have been made since the last check. We expect
C     25% of the computation to take longer than XWAIT seconds, so we
C     should see a report line for every 25% progress.
C
      FRAC   = XCHECK * INCR / XTOTAL

      NRPORT = NINT( XTOTAL / INCR )


      DO I = 1, NRPORT

         CALL ZZGFWKIN ( INCR )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     Rewind the file to set the record pointer to the start
C     of the file.
C
      REWIND ( XUNIT, IOSTAT = IOSTAT )
      CALL CHCKSI ( 'REWIND IOSTAT', IOSTAT, '=', 0,  0,  OK )

      IF ( IOSTAT .EQ. 0 ) THEN
C
C        The first two lines of the file should be blank.
C
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

C
C     If we haven't hit an error, read the rest of the file.
C
C     The expected number of status lines in the report is the
C     number of update calls divided by the call check ratio,
C     plus one, since the first line shows 0% progress.
C
      XNL = 1 + ( NRPORT / XCHECK )


      IF ( IOSTAT .EQ. 0 ) THEN

         DO I = 1, XNL

            QNAME = 'Log file line no. * IOSTAT'
            CALL REPMI ( QNAME, '*', I+2, QNAME )

            READ ( XUNIT, '(A)', IOSTAT = IOSTAT ) LINE
            CALL CHCKSI ( QNAME, IOSTAT, '=', 0,  0,  OK )


            IF ( IOSTAT .EQ. 0 ) THEN
C
C              Check the report line.
C
               CALL DPFMT ( (I-1)*100*FRAC, 'XXX.XX', PCTSTR )

               XLINE = XBEGMS
               CALL SUFFIX ( PCTSTR, 1, XLINE )
               CALL SUFFIX ( '%',    0, XLINE )
               CALL SUFFIX ( XENDMS, 1, XLINE )

               CALL CHCKSC ( 'LINE', LINE, '=', XLINE, OK )

            END IF

         END DO

      END IF


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Clean up: delete log file.' )

      IF ( EXISTS(REPFIL) ) THEN

         CLOSE ( XUNIT )

         CALL DELFIL( REPFIL )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF

C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Call to umbrella routine' )

      CALL ZZGFRPWK ( 1, 2.D0, 3.D0, 7, ' ', ' ', 0.5D0 )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )




      CALL T_SUCCESS ( OK )
      RETURN
      END
