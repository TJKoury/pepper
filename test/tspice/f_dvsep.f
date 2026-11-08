C$Procedure F_DVSEP ( DVSEP family tests )

      SUBROUTINE F_DVSEP ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        DVSEP
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
C     None.
C
C$ Particulars
C
C     None.
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
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 01-APR-2009 (EDW)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      DVSEP
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      T_RANDD
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      

C     
C     Local parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-12 )

      DOUBLE PRECISION      VAL
      DOUBLE PRECISION      COLAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MAG_LOG
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      VEL   ( 3 )
      DOUBLE PRECISION      REC1  ( 3 )
      DOUBLE PRECISION      REC2  ( 3 )
      DOUBLE PRECISION      CRSS1 ( 3 )
      DOUBLE PRECISION      CRSS2 ( 3 )
      DOUBLE PRECISION      S1    ( 6 )
      DOUBLE PRECISION      S2    ( 6 )
      DOUBLE PRECISION      S1_T  ( 6 )
      DOUBLE PRECISION      S2_T  ( 6 )
      DOUBLE PRECISION      TRANS ( 6,6 )

      INTEGER               I
      INTEGER               SEED1
      INTEGER               SEED2

      CHARACTER*(128)        TXT

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'dvsep.pck' )


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DVSEP' )

C
C     Create a PCK, load using FURNSH.
C
      CALL T_PCK08 ( PCK, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Case 1
C
      CALL TCASE ( 'DVSEP overflow.' )

C
C     Two state vectors, one rather odd.
C
      S1(1) = 3.D0 
      S1(2) = 4.D0 
      S1(3) = 0.D0
      S1(4) = -9.D0 
      S1(5) = 2.D0
      S1(6) = 0.1D0 

C
C     S2 has a very high derivative of the unit vector.
C
      S2(1) = 1.D0
      S2(2) = 0.D0
      S2(3) = 1.D0
      S2(4) = 1.D0 
      S2(5) = 0.9D0*DPMAX() 
      S2(6) = 0.D0

      VAL = DVSEP( S1, S2)
      CALL CHCKXC ( .TRUE., 'SPICE(NUMERICOVERFLOW)', OK )



C
C     Case 2
C
      CALL TCASE ( 'DVSEP S1xS2 = 0' )

      S1(1) = 25.D0
      S1(2) = 0.D0
      S1(3) = 0.D0
      S1(4) = 2.D0  
      S1(5) =-1.D0
      S1(6) = 1.D0

      S2(1) = -S1(1)
      S2(2) = -S1(2)
      S2(3) = -S1(3)
      S2(4) = -S1(4)
      S2(5) = -S1(5)
      S2(6) = -S1(6)

      VAL = DVSEP( S1, S2)
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Expect a separation angle rate of zero.
C
      CALL CHCKSD ( 'DVSEP S1xS2 = 0 (1)', VAL, '=', 0.D0, TIGHT, OK )


C
C     Case 3
C


C
C     Create two random vectors in R3, REC1 and REC2, each with 
C     magnitude MAG. Use the cross product operations 
C
C        CRSS1 = REC1  x REC2
C        CRSS2 = CRSS1 x REC1
C
C     to construct a velocity unit vector orthogonal to REC1 in the  
C     plane of REC1 and REC2 oriented such that REC1 rotates towards
C     REC2 with REC2 constant.
C
C     In this case
C
C            d(theta) 
C        v = -------- * || REC1 ||
C            dt
C
C     with v = 1, then
C
C        d(theta)     -1           -1
C        -------- =  ______    =   ---
C        dt 
C                  || REC1 ||      MAG
C 

      SEED1  = -82763653
      SEED2  = -273661


C
C     Define an arbitrary rotation from J2000 to something
C     not referenced against earth rotation at an arbitrary
C     ephemeris time, ET = 0 in this case.
C
      CALL SXFORM( 'J2000', 'IAU_MOON', 0.D0, TRANS )

C
C     Note, 5000 is also an arbitrary value.
C
      DO I = 1, 5000
      
         TXT = 'S1, S2 random test #, MAG_LOG #'

         CALL REPMI ( TXT, '#', I, TXT )

         MAG_LOG = T_RANDD( -2.D0, 25.D0, SEED1 )
         MAG     = 10**MAG_LOG

         CALL REPMD ( TXT, '#', MAG_LOG, 8, TXT )

         CALL TCASE ( TXT )

         COLAT = T_RANDD( 0.D0, PI(),    SEED1 )
         LON   = T_RANDD( 0.D0, TWOPI(), SEED2 )

         CALL SPHREC ( MAG, COLAT, LON, REC1 )

         COLAT = T_RANDD( 0.D0, PI(),    SEED1 )
         LON   = T_RANDD( 0.D0, TWOPI(), SEED2 )

         CALL SPHREC ( MAG, COLAT, LON, REC2 )

         CALL VCRSS ( REC1, REC2, CRSS1)
         CALL VCRSS ( CRSS1, REC1, CRSS2 )

         CALL VHAT ( CRSS2, VEL )

         CALL VPACK ( REC1(1),  REC1(2),  REC1(3), S1(1) )
         CALL VPACK ( VEL(1),   VEL(2),   VEL(3),  S1(4) )

         CALL VPACK ( REC2(1), REC2(2),  REC2(3),  S2(1) )
         CALL VPACK ( 0.D0,    0.D0,     0.D0,     S2(4) )

C
C        Apply the transformation, TRANS, to S1 and S2, creating new 
C        vectors without or with fewer zero components, particularly 
C        the S2 velocity components.
C
         CALL MXVG ( TRANS, S1, 6, 6, S1_T ) 
         CALL MXVG ( TRANS, S2, 6, 6, S2_T )

         VAL = DVSEP( S1_T, S2_T )

         CALL CHCKSD ( TXT, VAL, '~', -1.D0/MAG, TIGHT, OK )

      END DO

C
C Case n
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END





