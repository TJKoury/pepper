!$Procedure      F_GFRPRT ( Test GFRPRT entry points )

      SUBROUTINE F_GFRPRT ( OK )

! $ Abstract
!
!     Test the GF progress reporting entry points contained in GFRPRT.
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
!-    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB)
!
!-&


!
!     SPICELIB functions
!
      INTEGER               WNCARD

      LOGICAL               EXISTS

!
!     Local parameters
!
      CHARACTER*(*)         REPFIL
      PARAMETER           ( REPFIL = 'zzgfrpwk.txt' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )


      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 400 )

!
!     Local Variables
!
      CHARACTER*(MXBEGM)    BEGMSG
      CHARACTER*(MXENDM)    ENDMSG
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(MXBEGM)    XBEGMS
      CHARACTER*(MXENDM)    XENDMS

      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      FREQ
      DOUBLE PRECISION      INCR
      DOUBLE PRECISION      SPAN
      DOUBLE PRECISION      START
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TOTAL
      DOUBLE PRECISION      XFREQ
      DOUBLE PRECISION      WINDOW ( LBCELL : MAXWIN )
      DOUBLE PRECISION      XINCR
      DOUBLE PRECISION      XTOTAL

      INTEGER               I
      INTEGER               J
      INTEGER               NSAMP
      INTEGER               TCHECK
      INTEGER               UNIT
      INTEGER               XCHECK
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
      CALL TOPEN ( 'F_GFRPRT' )






!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test GFREPI: Make sure correct parameters are ' &
      //           'passed to ZZGFTSWK.'                            )


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

!
!     Create a confinement window.
!
      CALL SSIZED ( MAXWIN, WINDOW )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, MAXWIN-1, 2

         CALL WNINSD ( DBLE(I), DBLE(I+1), WINDOW )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL CHCKXC ( .FALSE., ' ', OK )

      XTOTAL = MAXWIN / 2.D0

      XBEGMS = '@--------------------Message prefix-------------------@'
      XENDMS = '@**suffix***@'

!
!     Call the progress report initializer.
!
      CALL GFREPI ( WINDOW, XBEGMS, XENDMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Fetch the current report parameters from the ZZGFRPWK monitor.
!
      CALL ZZGFWKMO ( UNIT, TOTAL, FREQ, TCHECK, BEGMSG, ENDMSG, INCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the stored values from ZZGFWKMO against the expected
!     values.
!
!     The default poll duration and call count are 1 second and
!     4 calls, respectively. These values are hard-coded in GFREPI.
!
      XFREQ  = 1.D0
      XCHECK = 4

      CALL CHCKSI ( 'UNIT',   UNIT,   '=', XUNIT,  0,      OK )
      CALL CHCKSD ( 'TOTAL',  TOTAL,  '~', XTOTAL, VTIGHT, OK )
      CALL CHCKSD ( 'FREQ',   FREQ,   '=', XFREQ,  0.D0,   OK )
      CALL CHCKSI ( 'TCHECK', TCHECK, '=', XCHECK, 0,      OK )
      CALL CHCKSC ( 'BEGMSG', BEGMSG, '=', XBEGMS,         OK )
      CALL CHCKSC ( 'ENDMSG', ENDMSG, '=', XENDMS,         OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test GFREPU: Make sure correct work increments are ' &
      //           'passed to ZZGFWKIN.'                               )


      DO I = 1, WNCARD( WINDOW )

         CALL WNFETD ( WINDOW, I, START, FINISH )

         SPAN  = FINISH - START

!
!        Use progressively smaller samples.
!
         NSAMP = I + 2

         DELTA = SPAN / (NSAMP-1)

         DO J = 1, NSAMP

            T = START + (J-1) * DELTA

            CALL GFREPU ( START, FINISH, T )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL ZZGFWKMO ( UNIT,   TOTAL,  FREQ, &
                           TCHECK, BEGMSG, ENDMSG, INCR )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           We expect the last work increment to be DELTA for all
!           but the first value of T.
!
            IF ( J .EQ. 1 ) THEN
               XINCR = 0.D0
            ELSE
               XINCR = DELTA
            END IF

            QNAME = 'INCR I=1*; J=*'

            CALL REPMI ( QNAME, '*', I, QNAME )
            CALL REPMI ( QNAME, '*', J, QNAME )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( QNAME, INCR, '~', XINCR, VTIGHT, OK )

         END DO

      END DO



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Test GFREPF: Make sure correct parameters ' &
      //           'are passed to ZZGFWKAD and ZZGFWKIN.'       )



      CALL GFREPF
      CALL CHCKXC ( .FALSE., ' ', OK )


!     Fetch the current report parameters from the ZZGFRPWK monitor.
!
      CALL ZZGFWKMO ( UNIT, TOTAL, FREQ, TCHECK, BEGMSG, ENDMSG, INCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the stored values from ZZGFWKMO against the expected
!     values.
!
!     GFREPF is supposed to set the poll duration and call count to 0
!     seconds and 1 call, respectively. The work increment should be
!     set to 0.D0. These values are hard-coded in GFREPF.
!
      XFREQ  = 0.D0
      XCHECK = 1

      CALL CHCKSI ( 'UNIT',   UNIT,   '=', XUNIT,  0,      OK )
      CALL CHCKSD ( 'TOTAL',  TOTAL,  '~', XTOTAL, VTIGHT, OK )
      CALL CHCKSD ( 'FREQ',   FREQ,   '=', XFREQ,  0.D0,   OK )
      CALL CHCKSI ( 'TCHECK', TCHECK, '=', XCHECK, 0,      OK )
      CALL CHCKSC ( 'BEGMSG', BEGMSG, '=', XBEGMS,         OK )
      CALL CHCKSC ( 'ENDMSG', ENDMSG, '=', XENDMS,         OK )



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

      CALL GFRPRT ( WINDOW, ' ', ' ', 0.D0, 1.D0, 0.5D0 )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'GFREPI: report message prefix is too long' )

      CALL GFREPI ( WINDOW, &
                    '@---------------------------------------------' &
      //            '---------------------------------------------#', &
                    ' '                                                )

      CALL CHCKXC ( .TRUE., 'SPICE(MESSAGETOOLONG)', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'GFREPI: report message suffix is too long' )

      CALL GFREPI ( WINDOW, &
                    ' ', &
                    '@---------------------------------------------' &
      //            '---------------------------------------------#'  )

      CALL CHCKXC ( .TRUE., 'SPICE(MESSAGETOOLONG)', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'GFREPI: report message prefix contains ' &
      //           'non-printing characters.'                )

      CALL GFREPI ( WINDOW,  '@-------#'//CHAR(4),  ' ' )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'GFREPI: report message suffix contains ' &
      //           'non-printing characters.'                )

      CALL GFREPI ( WINDOW,  ' ', '#-------!'//CHAR(5) )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'GFREPU: interval bounds out of order.'  )

      CALL GFREPU ( 1.D0, 0.D0, 2.D0 )

      CALL CHCKXC ( .TRUE., 'SPICE(BADENDPOINTS)', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'GFREPU: input time outside of interval.'  )

      CALL GFREPU ( 1.D0, 1.5D0, 2.D0 )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END




