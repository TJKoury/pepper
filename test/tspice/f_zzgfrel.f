C$Procedure      F_ZZGFREL ( Test ZZGFREL )
  
      SUBROUTINE F_ZZGFREL ( OK )

C$ Abstract
C
C     Test the GF scalar root finder ZZGFREL.
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

      INCLUDE              'gf.inc'

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
C     This family tests the GF scalar root finder ZZGFREL.
C      
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     Progress reporting and interrupt handling are not currently tested
C     in this version of this this test family, but test cases for these
C     features should be added.
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
C-    TSPICE Version 1.1.0, 26-APR-2012 (EDW)
C
C        Edited the invalid tolerance error test return
C        short message from SPICE(VALUEOUTOFRANGE) to 
C        SPICE(INVALIDTOLERANCE).
C
C-    SPICELIB Version 1.0.0, 28-AUG-2008 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI

      INTEGER               WNCARD

      LOGICAL               GFBAIL

C
C     External routines
C
      EXTERNAL              GFBAIL
      EXTERNAL              GFREFN
      EXTERNAL              GFREPF
      EXTERNAL              GFREPI
      EXTERNAL              GFREPU
      EXTERNAL              GFSTEP
      EXTERNAL              T_ZZGFREL_DEC
      EXTERNAL              T_ZZGFREL_GET
      EXTERNAL              T_ZZGFREL_LT
      EXTERNAL              T_ZZGFREL_UP


C     
C     Local parameters
C
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 100 )

C
C     Local Variables
C
      CHARACTER*(LNSIZE)    QNAME
      CHARACTER*(LNSIZE)    RELATE
      CHARACTER*(LNSIZE)    SRCPRE ( 2 )
      CHARACTER*(LNSIZE)    SRCSUF ( 2 )

      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      CRITPT ( MAXWIN )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      E
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      FTOL
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      SHORT  ( LBCELL : 1      )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEPSZ
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWMAX )
      DOUBLE PRECISION      XBEG   ( MAXWIN/2 )
      DOUBLE PRECISION      XBEGVL ( MAXWIN/2 )
      DOUBLE PRECISION      XEND   ( MAXWIN/2 )
      DOUBLE PRECISION      XENDVL ( MAXWIN/2 )
      DOUBLE PRECISION      XTIME  ( MAXWIN/2 )
      DOUBLE PRECISION      XVAL   ( MAXWIN/2 )

      INTEGER               I
      INTEGER               J
      INTEGER               MW
      INTEGER               N
      INTEGER               NCRIT
      INTEGER               NW
      
      LOGICAL               BAIL
      LOGICAL               RPT
 
C
C     Save everything.
C
      SAVE
 
C
C     Initial values
C
 

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFREL' )




C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Too few workspace windows.'  )

      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -PI(), PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = '='
      TOL       = 1.D-6

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL T_ZZGFREL_UP  ( REFVAL   )

      MW = MAXWIN
      NW = 4
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .TRUE., 'SPICE(TOOFEWWINDOWS)', OK )

      

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Workspace windows too short.'  )

      CALL SSIZED ( 1, SHORT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SHORT(1) = PI()

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = '='
      TOL       = 1.D-6


      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL T_ZZGFREL_UP  ( REFVAL   )

      MW = 1
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        SHORT,         MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)', OK )

      
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Result window too short; found before search'  )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -PI(), PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( 1, SHORT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = '='
      TOL       = 1.D-6

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL T_ZZGFREL_UP  ( REFVAL   )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        SHORT                       )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Result window too short; found during search'  )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -10*PI(), 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( 12, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     This case requires an actual search, so set the step size.
C
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = 'LOCMAX'
      TOL       = 1.D-6

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL T_ZZGFREL_UP  ( REFVAL   )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                     )

      CALL CHCKXC ( .TRUE., 'SPICE(OUTOFROOM)', OK )



      
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad adjustment value'  )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -PI(), PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  -1.D0
      REFVAL    =  0.5D0
      RELATE    = 'ABSMAX'
      TOL       = 1.D-6

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL T_ZZGFREL_UP  ( REFVAL   )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Non-positive tolerance value'  )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -PI(), PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = 'ABSMAX'
      TOL       = -1.D-6

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL T_ZZGFREL_UP  ( REFVAL   )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )



      TOL = 0.D0

      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad relational operator'  )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -PI(), PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = '=='

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL T_ZZGFREL_UP  ( REFVAL   )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )



C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Solve SIN(X) == 0.5 on [-pi,pi]' )
 
C
C     This case requires an actual search, so set the step size.
C
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -PI(), PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = '='

C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect two roots.
C
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

C
C     Check the times of and function values at the roots.
C
      XTIME(1) =     PI()/6
      XTIME(2) = 5 * PI()/6

      XVAL(1)  =     0.5D0
      XVAL(2)  =     0.5D0


      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XTIME(I), TOL, OK )

C
C           Make sure stop time exactly matches start time.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '=', START, 0.D0, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XVAL(I), FTOL, OK )

         END DO

      END IF
 



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Solve SIN(X) < 0.5 on [-pi,pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -PI(), PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = '<'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect two solution intervals.
C
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

C
C     Check the times of and function values at the roots.
C
      XBEG(1)   =    -PI()
      XEND(1)   =     PI()/6
      XBEG(2)   = 5 * PI()/6
      XEND(2)   =     PI()

      XBEGVL(1) =     0.D0
      XENDVL(1) =     0.5D0
      XBEGVL(2) =     0.5D0
      XENDVL(2) =     0.D0


      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(I), FTOL, OK )

C
C           Check stop time and value.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '~', XEND(I), TOL, OK )

            QNAME = 'Stop value #'
            CALL REPMI ( QNAME, '#', I, QNAME )


            CALL T_ZZGFREL_GET( FINISH, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '~', XENDVL(I), FTOL, OK )

         END DO

      END IF



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Solve SIN(X) > 0.5 on [-pi,pi]' )


      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -PI(), PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = '>'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect one solution interval.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C     Check the times of and function values at the roots.
C
      XBEG(1)   =     PI()/6
      XEND(1)   = 5 * PI()/6

      XBEGVL(1) =     0.5D0
      XENDVL(1) =     0.5D0


      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(I), FTOL, OK )

C
C           Check stop time and value.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '~', XEND(I), TOL, OK )

            QNAME = 'Stop value #'
            CALL REPMI ( QNAME, '#', I, QNAME )


            CALL T_ZZGFREL_GET( FINISH, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '~', XENDVL(I), FTOL, OK )

         END DO

      END IF


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find local maxima of SIN(X) on [0,10*pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 0.D0, 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.D0
      REFVAL    =  0.D0
      RELATE    = 'LOCMAX'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect 5 solutions.
C
      CALL CHCKSI ( 'N', N, '=', 5, 0, OK )

      DO I = 1, 5

         XBEG(I)   = (I-1)*2*PI() + PI()/2
         XEND(I)   = XBEG(I)

         XBEGVL(I) = 1.D0

      END DO

C
C     Check the times of and function values at the roots.
C
      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(I), FTOL, OK )

C
C           Check that stop time equals start time.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '=', START, 0.D0, OK )

         END DO

      END IF


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find local minima of SIN(X) on [0,10*pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 0.D0, 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.D0
      REFVAL    =  0.0D0
      RELATE    = 'LOCMIN'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect 5 solutions.
C
      CALL CHCKSI ( 'N', N, '=', 5, 0, OK )

      DO I = 1, 5

         XBEG(I)   = (I-1)*2*PI() + 3*PI()/2
         XEND(I)   = XBEG(I)

         XBEGVL(I) = -1.D0

      END DO

C
C     Check the times of and function values at the roots.
C
      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(I), FTOL, OK )

C
C           Check that stop time equals start time.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '=', START, 0.D0, OK )

         END DO

      END IF

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find adjusted absolute maxima of SIN(X) '
     .//           'on [0,10*pi]'                           )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 0.D0, 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.5D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMAX'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect 5 solutions.
C
      CALL CHCKSI ( 'N', N, '=', 5, 0, OK )

      DO I = 1, 5

         XBEG(I)   = (I-1)*2*PI() +   PI()/6
         XEND(I)   = (I-1)*2*PI() + 5*PI()/6

         XBEGVL(I) = 0.5D0
         XENDVL(I) = 0.5D0

      END DO

C
C     Check the times of and function values at the roots.
C
      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(I), FTOL, OK )
 
C
C           Check stop time.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '~', XEND(I), TOL, OK )

C
C           Check function value at stop time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Stop value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( FINISH, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XENDVL(I), FTOL, OK )

         END DO

      END IF
 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find adjusted absolute minima of SIN(X) '
     .//           'on [0,10*pi]'                           )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 0.D0, 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.5D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMIN'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect 5 solutions.
C
      CALL CHCKSI ( 'N', N, '=', 5, 0, OK )

      DO I = 1, 5

         XBEG(I)   = (I-1)*2*PI() +  7*PI()/6
         XEND(I)   = (I-1)*2*PI() + 11*PI()/6

         XBEGVL(I) = -0.5D0
         XENDVL(I) = -0.5D0

      END DO

C
C     Check the times of and function values at the roots.
C
      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(I), FTOL, OK )
 
C
C           Check stop time.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '~', XEND(I), TOL, OK )

C
C           Check function value at stop time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Stop value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( FINISH, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XENDVL(I), FTOL, OK )
 
         END DO

      END IF
 



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find absolute maximum of X on [-10*pi,10*pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -10*PI(), 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMAX'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'X' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect 1 solution.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      XBEG(1)   = 10*PI()
      XEND(1)   = XBEG(1)

      XBEGVL(1) = XBEG(1)

C
C     Check the times of and function values at the roots.
C
      IF ( OK ) THEN
 
         CALL WNFETD ( RESULT, 1, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C        Check start time.
C
         QNAME = 'Start time #'
         CALL REPMI ( QNAME, '#', I, QNAME )
 
         CALL CHCKSD ( QNAME, START, '~', XBEG(1), TOL, OK )
 
C
C        Check function value at start time. The function
C        has derivative magnitude not exceeding 1, so the
C        function tolerance can be set equal to the time
C        tolerance.
C
         QNAME = 'Start value #'
         CALL REPMI ( QNAME, '#', I, QNAME )
 
         CALL T_ZZGFREL_GET( START, VALUE )
 
         FTOL = TOL
 
         CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(1), FTOL, OK )
 
C
C        Check stop time.
C
         QNAME = 'Stop time #'
         CALL REPMI ( QNAME, '#', I, QNAME )
 
         CALL CHCKSD ( QNAME, FINISH, '=', START, 0.D0, OK )

      END IF
 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find absolute minimum of X on [-10*pi,10*pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -10*PI(), 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMIN'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'X' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect 1 solution.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      XBEG(1)   = -10*PI()
      XEND(1)   = XBEG(1)

      XBEGVL(1) = XBEG(1)

C
C     Check the times of and function values at the roots.
C
      IF ( OK ) THEN
 
         CALL WNFETD ( RESULT, 1, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C        Check start time.
C
         QNAME = 'Start time #'
         CALL REPMI ( QNAME, '#', I, QNAME )
 
         CALL CHCKSD ( QNAME, START, '~', XBEG(1), TOL, OK )
 
C
C        Check function value at start time. The function
C        has derivative magnitude not exceeding 1, so the
C        function tolerance can be set equal to the time
C        tolerance.
C
         QNAME = 'Start value #'
         CALL REPMI ( QNAME, '#', I, QNAME )
 
         CALL T_ZZGFREL_GET( START, VALUE )
 
         FTOL = TOL
 
         CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(1), FTOL, OK )
 
C
C        Check stop time.
C
         QNAME = 'Stop time #'
         CALL REPMI ( QNAME, '#', I, QNAME )
 
         CALL CHCKSD ( QNAME, FINISH, '=', START, 0.D0, OK )

      END IF
 



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find critical points of (X**2)SIN(X) '
     .//           'on [-10*pi,10*pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -10*PI(), 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = '='
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to the derivative of (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( 'D((X**2)*SIN(X))' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     If we had infinite precision, we would expect 21 solution 
C     points: there is a critical point at 0, since the function 
C
C         2
C        X  * SIN(X)
C
C     is asymptotic to X**3 as X -> 0. However, the root at 0
C     is a local minimum of the derivative: the derivative is
C     tangent to the X-axis at 0. So the GF system is unlikely
C     to be able to to detect this root. We require only that
C     at least 20 roots are found.
C
C
      CALL CHCKSI ( 'N', N, '>', 19, 0, OK )

      NCRIT = N

      DO I = 1, N

         CALL WNFETD ( RESULT, I, START, FINISH )
         CRITPT(I) = START

      END DO

C
C     If we did find a critical point at zero, delete it from
C     the set of critical points.
C
      IF ( NCRIT .EQ. 21 ) THEN

         DO I = 10, 20
            CRITPT(I) = CRITPT(I+1)
         END DO

         NCRIT = 20

      END IF

C
C     Check function values, but not the times, at the roots.
C
      IF ( OK ) THEN
 
         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check function value at start time. The function
C           has derivative amplitude on the order of 100, so the
C           function tolerance must be at least 2 orders of magnitude
C           looser than the time tolerance. Use 3 orders of magnitude.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL * 1000.D0

            CALL CHCKSD ( QNAME, VALUE, '~', 0.D0, FTOL, OK )

         END DO

      END IF
 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find local maxima of (X**2)SIN(X) '
     .//           'on [-10*pi,10*pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -10*PI(), 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'LOCMAX'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 10 local maxima.
C
      CALL CHCKSI ( 'N', N, '=', 10, 0, OK )

C
C     Set the expected start times. Local maxima occur between
C     0 and pi and in intervals offset from [0,pi] by multiples
C     of 2*pi.
C
      J = 1

      DO I = 1, 20, 2

         XBEG(J) = CRITPT(I)
         J       = J + 1

      END DO

C
C     Check the start times for the local maxima.
C
      IF ( OK ) THEN
 
         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

         END DO

      END IF

C
C     Check the 1st and 2nd derivative at each solution epoch.
C
      FTOL = 1.D-10

      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check the function at the start time; the function
C           should be positive.
C
            QNAME = 'Function at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '>', 0.D0, FTOL, OK )

C
C           Check the derivative at the start time.
C
            QNAME = 'Derivative at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( 'D((X**2)*SIN(X))' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '~', 0.D0, FTOL, OK )

C
C           Check the 2nd derivative at the start time.
C
            QNAME = '2nd derivative at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( 'D2((X**2)*SIN(X))' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '<', 0.D0, FTOL, OK )

         END DO

      END IF



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find absolute maximum of (X**2)SIN(X) '
     .//           'on [-10*pi,10*pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -10*PI(), 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMAX'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 1 absolute maximum.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C     Set the expected start time. Local maxima occur between
C     0 and pi and in intervals offset from [0,pi] by multiples
C     of 2*pi. Given the confinement interval we've picked,
C     the first local maximum should be the absolute
C     maximum.
C
      XBEG(1) = CRITPT(1)

C
C     Check the start time for the absolute maximum.
C
      IF ( OK ) THEN
 
         CALL WNFETD ( RESULT, 1, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check start time.
C
         QNAME = 'Start time #'
         CALL REPMI ( QNAME, '#', 1, QNAME )

         CALL CHCKSD ( QNAME, START, '~', XBEG(1), TOL, OK )

      END IF
 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find absolute maximum of (X**2)SIN(X) '
     .//           'on [-9*pi,10*pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -9*PI(), 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMAX'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 1 absolute maximum.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C     Set the expected start time. Local maxima occur between
C     0 and pi and in intervals offset from [0,pi] by multiples
C     of 2*pi. Given the confinement interval we've picked,
C     the last local maximum should be the absolute
C     maximum.
C
      XBEG(1) = CRITPT(19)

C
C     Check the start time for the absolute maximum.
C
      IF ( OK ) THEN
 
         CALL WNFETD ( RESULT, 1, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check start time.
C
         QNAME = 'Start time #'
         CALL REPMI ( QNAME, '#', 1, QNAME )

         CALL CHCKSD ( QNAME, START, '~', XBEG(1), TOL, OK )

      END IF
 



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find local minima of (X**2)SIN(X) '
     .//           'on [-10*pi,10*pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -10*PI(), 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'LOCMIN'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 10 local minima.
C
      CALL CHCKSI ( 'N', N, '=', 10, 0, OK )

C
C     Set the expected start times. Local minima occur between
C     -pi and 0 and in intervals offset from [-pi,0] by multiples
C     of 2*pi.
C
      J = 1

      DO I = 2, 20, 2

         XBEG(J) = CRITPT(I)
         J       = J + 1

      END DO

C
C     Check the start times for the local minima.
C
      IF ( OK ) THEN
 
         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

         END DO

      END IF

C
C     Check the 1st and 2nd derivative at each solution epoch.
C
      FTOL = 1.D-10

      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check the function at the start time; the function
C           should be negative.
C
            QNAME = 'Function at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '<', 0.D0, FTOL, OK )

C
C           Check the derivative at the start time.
C
            QNAME = 'Derivative at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( 'D((X**2)*SIN(X))' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '~', 0.D0, FTOL, OK )

C
C           Check the 2nd derivative at the start time.
C
            QNAME = '2nd derivative at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( 'D2((X**2)*SIN(X))' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '>', 0.D0, FTOL, OK )

         END DO

      END IF



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find absolute minimum of (X**2)SIN(X) '
     .//           'on [-10*pi,10*pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -10*PI(), 10*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMIN'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 1 absolute minimum.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C     Set the expected start time. Local minima occur between
C     -pi and 0 and in intervals offset from [-pi,0] by multiples
C     of 2*pi. Given the confinement interval we've picked,
C     the last local minimum should be the absolute
C     minimum.
C
      XBEG(1) = CRITPT(20)

C
C     Check the start time for the absolute maximum.
C
      IF ( OK ) THEN
 
         CALL WNFETD ( RESULT, 1, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check start time.
C
         QNAME = 'Start time #'
         CALL REPMI ( QNAME, '#', 1, QNAME )

         CALL CHCKSD ( QNAME, START, '~', XBEG(1), TOL, OK )

      END IF
 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find absolute minimum of (X**2)SIN(X) '
     .//           'on [-10*pi,9*pi]' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -10*PI(), 9*PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMIN'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 1 absolute minimum.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C     Set the expected start time. Local minima occur between
C     -pi and 0 and in intervals offset from [-pi,0] by multiples
C     of 2*pi. Given the confinement interval we've picked,
C     the first local minimum should be the absolute
C     minimum.
C
      XBEG(1) = CRITPT(2)

C
C     Check the start time for the absolute minimum.
C
      IF ( OK ) THEN
 
         CALL WNFETD ( RESULT, 1, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check start time.
C
         QNAME = 'Start time #'
         CALL REPMI ( QNAME, '#', 1, QNAME )

         CALL CHCKSD ( QNAME, START, '~', XBEG(1), TOL, OK )

      END IF
 


C*********************************************************************
C*
C*    Normal cases with multiple-interval confinement windows
C*
C*********************************************************************


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Solve SIN(X) == 0.5 on { [0,pi/6], [pi/3,pi/2], '
     .//           '[2pi/3,5pi/6] }'                                  )
 
C
C     This case requires an actual search, so set the step size.
C
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 0.D0,       PI()/6, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD (   PI()/3,   PI()/2, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 2*PI()/3, 5*PI()/6, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Expand the confinement window slightly to compensate for
C     round-off errors.
C
      E = 1.D-12

      CALL WNEXPD ( E, E, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = '='

C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect two roots.
C
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

C
C     Check the times of and function values at the roots.
C
      XTIME(1) =     PI()/6
      XTIME(2) = 5 * PI()/6

      XVAL(1)  =     0.5D0
      XVAL(2)  =     0.5D0


      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XTIME(I), TOL, OK )

C
C           Make sure stop time exactly matches start time.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '=', START, 0.D0, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XVAL(I), FTOL, OK )

         END DO

      END IF
 



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Solve SIN(X) < 0.5 on { [-pi/2,-pi/4], [0,pi/6], '
     .//           '[pi/3,2*pi/3], [5pi/6, pi] }'                      )
 
C
C     This case requires an actual search, so set the step size.
C
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -PI()/2,   -PI()/4, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 0.D0,       PI()/6, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD (   PI()/3,   3*PI()/3, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 5*PI()/6,     PI(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = '<'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect three solution intervals.
C
      CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

C
C     Check the times of and function values at the roots.
C
      XBEG(1)   =    -PI()/2
      XEND(1)   =    -PI()/4
      XBEG(2)   =     0.D0
      XEND(2)   =     PI()/6
      XBEG(3)   = 5 * PI()/6
      XEND(3)   =     PI()

      XBEGVL(1) =    -1.D0
      XENDVL(1) =    -SQRT(2.D0)/2
      XBEGVL(2) =     0.D0
      XENDVL(2) =     0.5D0
      XBEGVL(3) =     0.5D0
      XENDVL(3) =     0.D0


      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(I), FTOL, OK )

C
C           Check stop time and value.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '~', XEND(I), TOL, OK )

            QNAME = 'Stop value #'
            CALL REPMI ( QNAME, '#', I, QNAME )


            CALL T_ZZGFREL_GET( FINISH, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '~', XENDVL(I), FTOL, OK )

         END DO

      END IF


 


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Solve SIN(X) > 0.5 on { [-pi/2,0], [pi/6, pi/3], '
     .//           '[2*pi/3, 5*pi/6], [pi, 7*pi/6 }'                 )
 
C
C     This case requires an actual search, so set the step size.
C
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( -PI()/2,    0.D0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( PI()/6,    PI()/3, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( 2*PI()/3,  5*PI()/6, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( PI(),      7*PI()/6, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      ADJUST    =  0.D0
      REFVAL    =  0.5D0
      RELATE    = '>'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect two solution intervals.
C
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

C
C     Check the times of and function values at the roots.
C
      XBEG(1)   =     PI()/6
      XEND(1)   =     PI()/3
      XBEG(2)   = 2 * PI()/3
      XEND(2)   = 5 * PI()/6

      XBEGVL(1) =     0.5D0
      XENDVL(1) =     SQRT(3.D0)/2
      XBEGVL(2) =     SQRT(3.D0)/2
      XENDVL(2) =     0.5D0


      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(I), FTOL, OK )

C
C           Check stop time and value.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '~', XEND(I), TOL, OK )

            QNAME = 'Stop value #'
            CALL REPMI ( QNAME, '#', I, QNAME )


            CALL T_ZZGFREL_GET( FINISH, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '~', XENDVL(I), FTOL, OK )

         END DO

      END IF


 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find local maxima of (X**2)SIN(X) '
     .//           'on set of intervals bracketing critical points' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DELTA = 0.25D0

C
C     This confinement window contains 20 intervals. Make a check
C     here to guard against "code upgrades."
C
      CALL CHCKSI ( 'NCRIT', NCRIT, '=', 20, 0, OK )

      DO I = 1, NCRIT

         CALL WNINSD ( CRITPT(I)-DELTA, CRITPT(I)+DELTA, CNFINE )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'LOCMAX'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 10 local maxima.
C
      CALL CHCKSI ( 'N', N, '=', 10, 0, OK )

C
C     Set the expected start times. Local maxima occur between
C     0 and pi and in intervals offset from [0,pi] by multiples
C     of 2*pi.
C
      J = 1

      DO I = 1, 20, 2

         XBEG(J) = CRITPT(I)
         J       = J + 1

      END DO

C
C     Check the start times for the local maxima.
C
      IF ( OK ) THEN
 
         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

         END DO

      END IF

C
C     Check the 1st and 2nd derivative at each solution epoch.
C
      FTOL = 1.D-10

      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check the function at the start time; the function
C           should be positive.
C
            QNAME = 'Function at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '>', 0.D0, FTOL, OK )

C
C           Check the derivative at the start time.
C
            QNAME = 'Derivative at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( 'D((X**2)*SIN(X))' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '~', 0.D0, FTOL, OK )

C
C           Check the 2nd derivative at the start time.
C
            QNAME = '2nd derivative at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( 'D2((X**2)*SIN(X))' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '<', 0.D0, FTOL, OK )

         END DO

      END IF


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find local minima of (X**2)SIN(X) '
     .//           'on set of intervals bracketing critical points' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DELTA = 0.25D0

C
C     This confinement window contains 20 intervals. Make a check
C     here to guard against "code upgrades."
C
      CALL CHCKSI ( 'NCRIT', NCRIT, '=', 20, 0, OK )

      DO I = 1, NCRIT

         CALL WNINSD ( CRITPT(I)-DELTA, CRITPT(I)+DELTA, CNFINE )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'LOCMIN'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 10 local minima.
C
      CALL CHCKSI ( 'N', N, '=', 10, 0, OK )

C
C     Set the expected start times. Local minima occur between
C     -pi and 0 and in intervals offset from [-pi,0] by multiples
C     of 2*pi.
C
      J = 1

      DO I = 2, 20, 2

         XBEG(J) = CRITPT(I)
         J       = J + 1

      END DO

C
C     Check the start times for the local minima.
C
      IF ( OK ) THEN
 
         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

         END DO

      END IF

C
C     Check the 1st and 2nd derivative at each solution epoch.
C
      FTOL = 1.D-10

      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Check the function at the start time; the function
C           should be negative.
C
            QNAME = 'Function at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '<', 0.D0, FTOL, OK )

C
C           Check the derivative at the start time.
C
            QNAME = 'Derivative at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( 'D((X**2)*SIN(X))' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '~', 0.D0, FTOL, OK )

C
C           Check the 2nd derivative at the start time.
C
            QNAME = '2nd derivative at time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_SET ( 'D2((X**2)*SIN(X))' )
            CALL CHCKXC ( .FALSE., ' ', OK )
            
            CALL T_ZZGFREL_GET ( START, VALUE )

            CALL CHCKSD ( QNAME, VALUE, '>', 0.D0, FTOL, OK )

         END DO

      END IF




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find local maxima of (X**2)SIN(X) '
     .//           'on set of intervals excluding critical points' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DELTA = 0.25D0

C
C     This confinement window contains 20 intervals. Make a check
C     here to guard against "code upgrades."
C
      CALL CHCKSI ( 'NCRIT', NCRIT, '=', 20, 0, OK )

      DO I = 1, NCRIT

         CALL WNINSD ( CRITPT(I)+DELTA, CRITPT(I)+2*DELTA, CNFINE )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'LOCMAX'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find NO local maxima.
C
      CALL CHCKSI ( 'N', N, '=', 0, 0, OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find local minima of (X**2)SIN(X) '
     .//           'on set of intervals excluding critical points' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DELTA = 0.25D0

C
C     This confinement window contains 20 intervals. Make a check
C     here to guard against "code upgrades."
C
      CALL CHCKSI ( 'NCRIT', NCRIT, '=', 20, 0, OK )

      DO I = 1, NCRIT

         CALL WNINSD ( CRITPT(I)+DELTA, CRITPT(I)+2*DELTA, CNFINE )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'LOCMIN'

C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find NO local minima.
C
      CALL CHCKSI ( 'N', N, '=', 0, 0, OK )





C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find absolute maximum of (X**2)SIN(X) '
     .//           'on { [-10*pi,-9*pi], [-8pi,-7pi],...,[8pi,9pi] }' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = -10, 8, 2
         CALL WNINSD ( I*PI(), (I+1)*PI(), CNFINE )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMAX'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 1 absolute maximum.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C     Set the expected start time. Local maxima occur between
C     0 and pi and in intervals offset from [0,pi] by multiples
C     of 2*pi. Given the confinement interval we've picked,
C     the first local maximum should be the absolute
C     maximum.
C
      XBEG(1) = CRITPT(1)

C
C     Check the start time for the absolute maximum.
C
      IF ( OK ) THEN
 
         CALL WNFETD ( RESULT, 1, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check start time.
C
         QNAME = 'Start time #'
         CALL REPMI ( QNAME, '#', 1, QNAME )

         CALL CHCKSD ( QNAME, START, '~', XBEG(1), TOL, OK )

      END IF



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find absolute maximum of (X**2)SIN(X) '
     .//           'on { [-8*pi,-7*pi], [-6pi,-5pi],...,[8pi,9pi] }' )
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = -8, 8, 2
         CALL WNINSD ( I*PI(), (I+1)*PI(), CNFINE )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMAX'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 1 absolute maximum.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C     Set the expected start time. Local maxima occur between
C     0 and pi and in intervals offset from [0,pi] by multiples
C     of 2*pi. Given the confinement interval we've picked,
C     the last local maximum should be the absolute
C     maximum.
C
      XBEG(1) = CRITPT(19)

C
C     Check the start time for the absolute maximum.
C
      IF ( OK ) THEN
 
         CALL WNFETD ( RESULT, 1, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check start time.
C
         QNAME = 'Start time #'
         CALL REPMI ( QNAME, '#', 1, QNAME )

         CALL CHCKSD ( QNAME, START, '~', XBEG(1), TOL, OK )

      END IF

 



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find absolute minimum of (X**2)SIN(X) on '
     .//           '{ [-9*pi,-8*pi], [-8pi,-7pi],...,[9pi,10pi] }')
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = -9, 9, 2
         CALL WNINSD ( I*PI(), (I+1)*PI(), CNFINE )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMIN'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 1 absolute minimum.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C     Set the expected start time. Local minima occur between
C     -pi and 0 and in intervals offset from [-pi,0] by multiples
C     of 2*pi. Given the confinement interval we've picked,
C     the last local minimum should be the absolute
C     minimum.
C
      XBEG(1) = CRITPT(20)

C
C     Check the start time for the absolute maximum.
C
      IF ( OK ) THEN
 
         CALL WNFETD ( RESULT, 1, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check start time.
C
         QNAME = 'Start time #'
         CALL REPMI ( QNAME, '#', 1, QNAME )

         CALL CHCKSD ( QNAME, START, '~', XBEG(1), TOL, OK )

      END IF
 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find absolute minimum of (X**2)SIN(X) on '
     .//           '{ [-9*pi,-8*pi], [-8pi,-7pi],...,[7pi,8pi] }')
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = -9, 7, 2
         CALL WNINSD ( I*PI(), (I+1)*PI(), CNFINE )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.0D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMIN'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

C
C     Set the function to (X**2)*SIN(X).
C
      CALL T_ZZGFREL_SET ( '(X**2)*SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should find 1 absolute minimum.
C
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C     Set the expected start time. Local minima occur between
C     -pi and 0 and in intervals offset from [-pi,0] by multiples
C     of 2*pi. Given the confinement interval we've picked,
C     the first local minimum should be the absolute
C     minimum.
C
      XBEG(1) = CRITPT(2)

C
C     Check the start time for the absolute minimum.
C
      IF ( OK ) THEN
 
         CALL WNFETD ( RESULT, 1, START, FINISH )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check start time.
C
         QNAME = 'Start time #'
         CALL REPMI ( QNAME, '#', 1, QNAME )

         CALL CHCKSD ( QNAME, START, '~', XBEG(1), TOL, OK )

      END IF
 


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find adjusted absolute maxima of SIN(X) on '
     .//           '{ [-9*pi,-8*pi], [-7pi,-6pi],...,[7pi,8pi] }')
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = -9, 7, 2
         CALL WNINSD ( I*PI(), (I+1)*PI(), CNFINE )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.5D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMAX'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect 18 solutions.
C
      CALL CHCKSI ( 'N', N, '=', 18, 0, OK )

      DO I = 1, N, 2

         XBEG(I)     = (I-1)*PI() - 9*PI() 
         XEND(I)     = (I-1)*PI() - 9*PI() + PI()/6

         XBEGVL(I)   =  0.D0
         XENDVL(I)   = -0.5D0

         XBEG(I+1)   = (I-1)*PI() - 9*PI() + 5*PI()/6
         XEND(I+1)   = (I-1)*PI() - 8*PI() 

         XBEGVL(I+1) = -0.5D0
         XENDVL(I+1) =  0.D0

      END DO

C
C     Check the times of and function values at the roots.
C
      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(I), FTOL, OK )
 
C
C           Check stop time.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '~', XEND(I), TOL, OK )

C
C           Check function value at stop time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Stop value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( FINISH, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XENDVL(I), FTOL, OK )

         END DO

      END IF
 


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find adjusted absolute minima of SIN(X) on '
     .//           '{ [-9*pi,-8*pi], [-7pi,-6pi],...,[7pi,8pi] }')
 
      STEPSZ = 0.25D0
      CALL GFSSTP ( STEPSZ )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = -9, 7, 2
         CALL WNINSD ( I*PI(), (I+1)*PI(), CNFINE )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      ADJUST    =  0.5D0
      REFVAL    =  0.0D0
      RELATE    = 'ABSMIN'
C
C     We should be able to work with a very tight tolerance, since
C     our independent variable is close to zero.
C
      TOL       = 1.D-13

      RPT       =  .FALSE.
      BAIL      =  .FALSE.

      SRCPRE(1) = 'Search prefix 1'
      SRCPRE(2) = 'Search prefix 2'

      SRCSUF(1) = 'Search suffix 1'
      SRCSUF(2) = 'Search suffix 2'

      CALL T_ZZGFREL_SET ( 'SIN(X)' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_ZZGFREL_UP  ( REFVAL   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      MW = MAXWIN
      NW = NWMAX
      
      CALL ZZGFREL ( GFSTEP,        GFREFN,        T_ZZGFREL_DEC,  
     .               T_ZZGFREL_LT,  T_ZZGFREL_GET, T_ZZGFREL_UP,
     .               RELATE,        REFVAL,        TOL,
     .               ADJUST,        CNFINE,        MW,
     .               NW,            WORK,          RPT, 
     .               GFREPI,        GFREPU,        GFREPF,
     .               SRCPRE,        SRCSUF,        BAIL,
     .               GFBAIL,        RESULT                       )

      CALL CHCKXC ( .FALSE., ' ', OK )


      N = WNCARD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We expect 9 solutions.
C
      CALL CHCKSI ( 'N', N, '=', 9, 0, OK )

      DO I = 1, N

         XBEG(I)     = (I-1)*2*PI() - 9*PI() +   PI()/6
         XEND(I)     = (I-1)*2*PI() - 9*PI() + 5*PI()/6

         XBEGVL(I)   = -0.5D0
         XENDVL(I)   = -0.5D0

      END DO

C
C     Check the times of and function values at the roots.
C
      IF ( OK ) THEN

         DO I = 1, N

            CALL WNFETD ( RESULT, I, START, FINISH )
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Check start time.
C
            QNAME = 'Start time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, START, '~', XBEG(I), TOL, OK )

C
C           Check function value at start time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Start value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( START, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XBEGVL(I), FTOL, OK )
 
C
C           Check stop time.
C
            QNAME = 'Stop time #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL CHCKSD ( QNAME, FINISH, '~', XEND(I), TOL, OK )

C
C           Check function value at stop time. The function
C           has derivative magnitude not exceeding 1, so the
C           function tolerance can be set equal to the time
C           tolerance.
C
            QNAME = 'Stop value #'
            CALL REPMI ( QNAME, '#', I, QNAME )

            CALL T_ZZGFREL_GET( FINISH, VALUE )

            FTOL = TOL

            CALL CHCKSD ( QNAME, VALUE, '~', XENDVL(I), FTOL, OK )

         END DO

      END IF




      CALL T_SUCCESS ( OK )
      RETURN
      END











C*********************************************************************
C*
C*    External routines
C*
C*********************************************************************


C
C     Utility package umbrella:
C
      SUBROUTINE T_ZZGFREL_UTIL ( F, ET, REF, VALUE, DECRES, LSSTHN )
      IMPLICIT NONE

      CHARACTER*(*)         F
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      REF
      DOUBLE PRECISION      VALUE
      LOGICAL               DECRES
      LOGICAL               LSSTHN

C
C     SPICELIB functions
C
 
C
C     Local parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    SVFNAM

      DOUBLE PRECISION      DFVAL
      DOUBLE PRECISION      FVAL
      DOUBLE PRECISION      SVREF

C
C     Saved variables
C
      SAVE                  SVFNAM
      SAVE                  SVREF

C
C     Initial values
C
      DATA                  SVFNAM / ' '  /
      DATA                  SVREF  / 0.D0 /
      
      CALL CHKIN  ( 'T_ZZGFREL_UTIL'    )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_ZZGFREL_UTIL'    )
      RETURN


C
C     Get quantity at ET.
C
      ENTRY T_ZZGFREL_GET ( ET, VALUE )

      CALL CHKIN ( 'T_ZZGFREL_GET' )


      IF ( SVFNAM .EQ. 'X' ) THEN

         VALUE = ET

      ELSE IF ( SVFNAM .EQ. 'SIN(X)' ) THEN

         VALUE = SIN( ET )

      ELSE IF ( SVFNAM .EQ. '(X**2)*SIN(X)' ) THEN

         VALUE = ( ET**2 ) * SIN( ET )


      ELSE IF ( SVFNAM .EQ. 'D((X**2)*SIN(X))' ) THEN
C
C        This is the derivative of (X**2)*SIN(X).
C
         VALUE = 2*ET*SIN( ET ) + (ET**2)*COS(ET)

 
      ELSE IF ( SVFNAM .EQ. 'D2((X**2)*SIN(X))' ) THEN
C
C        This is the 2nd derivative of (X**2)*SIN(X).
C
C
         VALUE =     2*SIN(ET) 
     .            +  2*ET*COS(ET)
     .            +  2*ET*COS(ET)
     .            +  ET**2 * (-SIN(ET))


      ELSE 

         CALL SETMSG ( 'Bad function; function name is #.' )
         CALL ERRCH  ( '#', SVFNAM                         )
         CALL SIGERR ( 'SPICE(TSPICEBUG)'                  )

      END IF

      CALL CHKOUT ( 'T_ZZGFREL_GET' )
      RETURN



C
C     Update reference value.
C
      ENTRY T_ZZGFREL_UP ( REF )
      
 
      SVREF = REF

      RETURN


C
C     Indicate whether function is less than reference value
C     at specified epoch.
C
      ENTRY T_ZZGFREL_LT ( ET, LSSTHN )
 
      LSSTHN = .FALSE.

      IF ( SVFNAM .EQ. 'X' ) THEN

         FVAL = ET

      ELSE IF ( SVFNAM .EQ. 'SIN(X)' ) THEN

         FVAL = SIN( ET ) 


      ELSE IF ( SVFNAM .EQ. '(X**2)*SIN(X)' ) THEN

         FVAL = ( ET**2 ) * SIN( ET )


      ELSE IF ( SVFNAM .EQ. 'D((X**2)*SIN(X))' ) THEN
C
C        This is the derivative of (X**2)*SIN(X)
C
         FVAL = 2*ET*SIN( ET ) + (ET**2)*COS(ET)


      ELSE IF ( SVFNAM .EQ. 'D2((X**2)*SIN(X))' ) THEN
C
C        This is the 2nd derivative of (X**2)*SIN(X)
C
         FVAL =       2 * SIN(ET)
     .            +   2 * ET      * COS(ET)
     .            +   2 * ET      * COS(ET)
     .            +   ET**2       * (-SIN(ET))


      ELSE 

         CALL SETMSG ( 'Bad function; function name is #.' )
         CALL ERRCH  ( '#', SVFNAM                         )
         CALL SIGERR ( 'SPICE(TSPICEBUG)'                  )

      END IF

      LSSTHN = FVAL .LT. SVREF

      RETURN


C
C     Indicate whether function is decreasing
C     at specified epoch.
C
      ENTRY T_ZZGFREL_DEC ( ET, DECRES )
 
      DECRES = .FALSE.


      IF ( SVFNAM .EQ. 'X' ) THEN

         DFVAL = 1.D0

      ELSE IF ( SVFNAM .EQ. 'SIN(X)' ) THEN

         DFVAL = COS( ET ) 


      ELSE IF ( SVFNAM .EQ. '(X**2)*SIN(X)' ) THEN

         DFVAL = ( 2 * ET * SIN(ET) )  +  ( ( ET**2 ) * COS( ET ) )

C         WRITE (*,*) 'ET, DFVAL = ', ET, DFVAL

      ELSE IF ( SVFNAM .EQ. 'D((X**2)*SIN(X))' ) THEN
C
C        The function is the derivative of (X**2)*SIN(X).
C        DFVAL is the 2nd derivative of this function,
C        in other words the derivative of
C
C           2*X*SIN(X) +  (X**2)*COS(X)
C
         DFVAL =      (   2 * SIN(ET)                )        
     .             +  (   2 * ET      *    COS(ET)   )
     .             +  (   2 * ET      *    COS(ET)   ) 
     .             +  (   ET**2       * ( -SIN(ET) ) )


      ELSE 

         CALL SETMSG ( 'Bad function; function name is #.' )
         CALL ERRCH  ( '#', SVFNAM                         )
         CALL SIGERR ( 'SPICE(TSPICEBUG)'                  )

      END IF

      DECRES = DFVAL .LT. 0.D0

      RETURN


C
C     Select function.
C
      ENTRY T_ZZGFREL_SET ( F )
      
      CALL LJUST ( F,      SVFNAM )
      CALL UCASE ( SVFNAM, SVFNAM )

      RETURN
      END





