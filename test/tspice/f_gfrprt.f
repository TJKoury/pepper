C$Procedure      F_GFRPRT ( Test GFRPRT entry points )
  
      SUBROUTINE F_GFRPRT ( OK )

C$ Abstract
C
C     Test the GF progress reporting entry points contained in GFRPRT.
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
C-    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      INTEGER               WNCARD

      LOGICAL               EXISTS

C     
C     Local parameters
C
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

C
C     Local Variables
C
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
      CALL TOPEN ( 'F_GFRPRT' )



      


C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test GFREPI: Make sure correct parameters are '
     .//           'passed to ZZGFTSWK.'                            )


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

C
C     Create a confinement window.
C
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

C
C     Call the progress report initializer.
C
      CALL GFREPI ( WINDOW, XBEGMS, XENDMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Fetch the current report parameters from the ZZGFRPWK monitor.
C
      CALL ZZGFWKMO ( UNIT, TOTAL, FREQ, TCHECK, BEGMSG, ENDMSG, INCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the stored values from ZZGFWKMO against the expected
C     values.
C
C     The default poll duration and call count are 1 second and
C     4 calls, respectively. These values are hard-coded in GFREPI.
C
      XFREQ  = 1.D0
      XCHECK = 4

      CALL CHCKSI ( 'UNIT',   UNIT,   '=', XUNIT,  0,      OK )
      CALL CHCKSD ( 'TOTAL',  TOTAL,  '~', XTOTAL, VTIGHT, OK )  
      CALL CHCKSD ( 'FREQ',   FREQ,   '=', XFREQ,  0.D0,   OK )
      CALL CHCKSI ( 'TCHECK', TCHECK, '=', XCHECK, 0,      OK )
      CALL CHCKSC ( 'BEGMSG', BEGMSG, '=', XBEGMS,         OK )
      CALL CHCKSC ( 'ENDMSG', ENDMSG, '=', XENDMS,         OK )
      

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test GFREPU: Make sure correct work increments are '
     .//           'passed to ZZGFWKIN.'                               )


      DO I = 1, WNCARD( WINDOW )
         
         CALL WNFETD ( WINDOW, I, START, FINISH )

         SPAN  = FINISH - START

C
C        Use progressively smaller samples.
C
         NSAMP = I + 2

         DELTA = SPAN / (NSAMP-1)

         DO J = 1, NSAMP

            T = START + (J-1) * DELTA

            CALL GFREPU ( START, FINISH, T )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL ZZGFWKMO ( UNIT,   TOTAL,  FREQ, 
     .                     TCHECK, BEGMSG, ENDMSG, INCR )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           We expect the last work increment to be DELTA for all
C           but the first value of T.
C
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



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Test GFREPF: Make sure correct parameters '
     .//           'are passed to ZZGFWKAD and ZZGFWKIN.'       )



      CALL GFREPF
      CALL CHCKXC ( .FALSE., ' ', OK )


C     Fetch the current report parameters from the ZZGFRPWK monitor.
C
      CALL ZZGFWKMO ( UNIT, TOTAL, FREQ, TCHECK, BEGMSG, ENDMSG, INCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the stored values from ZZGFWKMO against the expected
C     values.
C
C     GFREPF is supposed to set the poll duration and call count to 0
C     seconds and 1 call, respectively. The work increment should be
C     set to 0.D0. These values are hard-coded in GFREPF.
C
      XFREQ  = 0.D0
      XCHECK = 1

      CALL CHCKSI ( 'UNIT',   UNIT,   '=', XUNIT,  0,      OK )
      CALL CHCKSD ( 'TOTAL',  TOTAL,  '~', XTOTAL, VTIGHT, OK )  
      CALL CHCKSD ( 'FREQ',   FREQ,   '=', XFREQ,  0.D0,   OK )
      CALL CHCKSI ( 'TCHECK', TCHECK, '=', XCHECK, 0,      OK )
      CALL CHCKSC ( 'BEGMSG', BEGMSG, '=', XBEGMS,         OK )
      CALL CHCKSC ( 'ENDMSG', ENDMSG, '=', XENDMS,         OK )



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

      CALL GFRPRT ( WINDOW, ' ', ' ', 0.D0, 1.D0, 0.5D0 )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'GFREPI: report message prefix is too long' )

      CALL GFREPI ( WINDOW, 
     .              '@---------------------------------------------'
     .//            '---------------------------------------------#', 
     .              ' '                                                )

      CALL CHCKXC ( .TRUE., 'SPICE(MESSAGETOOLONG)', OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'GFREPI: report message suffix is too long' )

      CALL GFREPI ( WINDOW, 
     .              ' ',
     .              '@---------------------------------------------'
     .//            '---------------------------------------------#'  )

      CALL CHCKXC ( .TRUE., 'SPICE(MESSAGETOOLONG)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'GFREPI: report message prefix contains '
     .//           'non-printing characters.'                )

      CALL GFREPI ( WINDOW,  '@-------#'//CHAR(4),  ' ' )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'GFREPI: report message suffix contains '
     .//           'non-printing characters.'                )

      CALL GFREPI ( WINDOW,  ' ', '#-------!'//CHAR(5) )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'GFREPU: interval bounds out of order.'  )

      CALL GFREPU ( 1.D0, 0.D0, 2.D0 )

      CALL CHCKXC ( .TRUE., 'SPICE(BADENDPOINTS)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'GFREPU: input time outside of interval.'  )

      CALL GFREPU ( 1.D0, 1.5D0, 2.D0 )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

 
      CALL T_SUCCESS ( OK )
      RETURN
      END




