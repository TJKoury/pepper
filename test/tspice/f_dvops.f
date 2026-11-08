C$Procedure F_DVOPS (Family of vector operations derivative tests)

      SUBROUTINE F_DVOPS ( OK )
 
C$ Abstract
C
C     This routine performs rudimentary tests on a collection
C     of derivative of vector operation
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
C-    TSPICE Version 2.0.0, 27-SEP-2005 (NJB)
C
C$ Version
C
C-   TSPICE Version 2.1.0, 03-MAY-2010 (EDW)
C
C       Added test family for DVNORM.
C
C-   TSPICE Version 2.0.0, 27-SEP-2005 (NJB)
C
C       Updated to remove non-standard use of duplicate arguments
C       in VSCL, VPERP, and UNORM.
C
C-&
 
C$ Index_Entries
C
C     test DVDOT, DVCRSS, DVHAT, DUCRSS, DVNORM
C
C-&

C
C     Test Utility Functions
C

 
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      DVDOT
      DOUBLE PRECISION      DVNORM
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      T_RANDD

C     
C     Local parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-13 )

C
C     Local Variables
C
      DOUBLE PRECISION      CMP
      DOUBLE PRECISION      EXP
      DOUBLE PRECISION      LENGTH
      DOUBLE PRECISION      MAG_LOG
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      LLIM
      DOUBLE PRECISION      ULIM
      DOUBLE PRECISION      DVMAG
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      X1     ( 3 )
      DOUBLE PRECISION      PRT1   ( 3 )
      DOUBLE PRECISION      PRT2   ( 3 )
      DOUBLE PRECISION      EXPV   ( 6 )
      DOUBLE PRECISION      S1     ( 6 )
      DOUBLE PRECISION      S2     ( 6 )
      DOUBLE PRECISION      SOUT   ( 6 )
 
      CHARACTER*(64)        TXT
 
      INTEGER               I
      INTEGER               SEED1

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DVOPS' )
 
      S1( 1 ) =  1.0D0
      S1( 2 ) =  2.0D0
      S1( 3 ) =  1.0D0
      S1( 4 ) =  1.0D0
      S1( 5 ) = -1.0D0
      S1( 6 ) =  3.0D0
 
      S2( 1 ) = 2.0D0
      S2( 2 ) = 3.0D0
      S2( 3 ) = 4.0D0
      S2( 4 ) = 1.0D0
      S2( 5 ) = 2.0D0
      S2( 6 ) = 3.0D0
 
C
C     Case 1
C
      CALL TCASE ( 'Test DVDOT.' )
 
 
         EXP =  S1(1) * S2(4)   +  S1(4)*S2(1)
     .       +  S1(2) * S2(5)   +  S1(5)*S2(2)
     .       +  S1(3) * S2(6)   +  S1(6)*S2(3)
 
         CMP = DVDOT( S1, S2 )
 
         CALL CHCKXC ( .FALSE.,      ' ',             OK )
         CALL CHCKSD ( 'DVDOT', CMP, '=', EXP, 0.0D0, OK )
 
 
C
C     Case 2
C
      CALL TCASE ( 'Test DVCRSS.' )
 
 
         CALL VCRSS ( S1(1),   S2  (1), EXPV(1) )
         CALL VCRSS ( S1(1),   S2  (4), PRT1(1) )
         CALL VCRSS ( S1(4),   S2  (1), PRT2(1) )
         CALL VADD  ( PRT1(1), PRT2(1), EXPV(4) )
 
         CALL CHCKXC ( .FALSE., ' ',  OK   )
         CALL DVCRSS (  S1,      S2,  SOUT )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'SOUT', SOUT, '=', EXPV, 6, 0.0D0, OK )
 
 
C
C     Case 3
C
      CALL TCASE ( 'Test DVHAT with a state having non-zero position.' )
 
         CALL UNORM  ( S1,           EXPV,    LENGTH  )
         CALL VPERP  ( S1(4),        EXPV,    EXPV(4) )
         CALL VSCLIP ( 1.0D0/LENGTH,          EXPV(4) )
 
         CALL DVHAT ( S1,                    SOUT    )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'SOUT', SOUT, '=', EXPV, 6, 0.0D0, OK )
 

C
C     Case 4
C
      CALL TCASE ( 'Test DVHAT with a state having zero position.' ) 
 
         S1(1)   = 0.0D0
         S1(2)   = 0.0D0
         S1(3)   = 0.0D0
 
         EXPV(1) = 0.0D0
         EXPV(2) = 0.0D0
         EXPV(3) = 0.0D0
         EXPV(4) = S1(4)
         EXPV(5) = S1(5)
         EXPV(6) = S1(6)
 
         CALL DVHAT  ( S1,            SOUT )
 
         CALL CHCKXC ( .FALSE., ' ',  OK   )
         CALL CHCKAD ( 'SOUT', SOUT, '=', EXPV, 6, 0.0D0, OK )
 

C
C     Case 5
C
      CALL TCASE ( 'Test DUCRSS' )
 
         S1( 1 ) =  1.0D0
         S1( 2 ) =  2.0D0
         S1( 3 ) =  1.0D0
 
         CALL VCRSS ( S1(1),   S2(1),        EXPV(1) )
         CALL VCRSS ( S1(1),   S2(4),        PRT1(1) )
         CALL VCRSS ( S1(4),   S2(1),        PRT2(1) )
         CALL VADD  ( PRT1(1), PRT2(1),      EXPV(4) )
         CALL UNORM ( EXPV,    VTEMP,        LENGTH  )
         CALL VEQU  ( VTEMP,                 EXPV    )
         CALL VPERP ( EXPV(4),      EXPV(1), VTEMP   )
         CALL VSCL  ( 1.0D0/LENGTH, VTEMP,   EXPV(4) )
 
         CALL DUCRSS ( S1, S2,  SOUT )
 
         CALL CHCKXC ( .FALSE., ' ',                      OK )
         CALL CHCKAD ( 'SOUT', SOUT, '=', EXPV, 6, 0.0D0, OK )
 


C     
C     Case 6
C     
C     Test DVNORM, parallel, anti-parallel, zero vector cases.
C     

      SEED1 = -82763653
      LLIM  = -2.D0
      ULIM  = 25.D0
      
      X1(1) = 1.D0
      X1(2) = DSQRT(2.D0)
      X1(3) = DSQRT(3.D0)

      TXT = 'DVNORM ZERO VECTOR CASE'
      CALL TCASE( TXT )

      S1( 1 ) =  0.D0
      S1( 2 ) =  0.D0
      S1( 3 ) =  0.D0
      S1( 4 ) =  X1(1)
      S1( 5 ) =  X1(2)
      S1( 6 ) =  X1(3)

      DVMAG = DVNORM(S1)
 
      CALL CHCKSD ( 'DVNORM', DVMAG, '=', 0.D0, TIGHT, OK )

C     
C     Create a set of 6x1 arrays with, the first three components
C     describing a vector with a random magnitude.
C     
C
C     Note, 5000 is an arbitrary value.
C
      DO I = 1, 5000

         MAG_LOG = T_RANDD( LLIM, ULIM, SEED1 )
         MAG     = 10.D0**MAG_LOG

         TXT = 'DVNORM parallel %d, MAG_LOG %f'
         CALL REPMI ( TXT, '%d', I, TXT )
         CALL REPMD ( TXT, '%f', MAG_LOG, 6, TXT )

         CALL TCASE ( TXT )

         S1( 1 ) =  X1(1) * MAG
         S1( 2 ) =  X1(2) * MAG
         S1( 3 ) =  X1(3) * MAG
         S1( 4 ) =  X1(1)
         S1( 5 ) =  X1(2)
         S1( 6 ) =  X1(3)
 
         DVMAG = DVNORM(S1)
 
         CALL CHCKSD ( TXT, DVMAG, '~', VNORM(X1), TIGHT, OK )


         MAG_LOG = T_RANDD( LLIM, ULIM, SEED1 )
         MAG     = 10.D0**MAG_LOG

         TXT = 'DVNORM anti-parallel %d, MAG_LOG %f'
         CALL REPMI ( TXT, '%d', I, TXT )
         CALL REPMD ( TXT, '%f', MAG_LOG, 6, TXT )

         CALL TCASE ( TXT )

         S1( 1 ) =  X1(1) * MAG
         S1( 2 ) =  X1(2) * MAG
         S1( 3 ) =  X1(3) * MAG
         S1( 4 ) =  -X1(1)
         S1( 5 ) =  -X1(2)
         S1( 6 ) =  -X1(3)
 
         DVMAG = DVNORM(S1)
 
         CALL CHCKSD ( TXT, DVMAG, '~', -VNORM(X1), TIGHT, OK )


         MAG_LOG = T_RANDD( LLIM, ULIM, SEED1 )
         MAG     = 10.D0**MAG_LOG

         TXT = 'DVNORM orthogonal %d, MAG_LOG %f'
         CALL REPMI ( TXT, '%d', I, TXT )
         CALL REPMD ( TXT, '%f', MAG_LOG, 6, TXT )

         CALL TCASE ( TXT )

C        
C        Create a 6x1 array with the property the dot product of the
C        first three components with the second three components
C        has value zero.
C     
         S2( 1 ) =  X1(1) * MAG
         S2( 2 ) =  X1(2) * MAG
         S2( 3 ) =  X1(3) * MAG
         S2( 4 ) =  1.D0/S2(1)
         S2( 5 ) =  1.D0/S2(2)
         S2( 6 ) = -2.D0/S2(3)
 
         DVMAG = DVNORM(S2)
 
         CALL CHCKSD ( TXT, DVMAG, '~', 0.D0, TIGHT, OK )


      END DO
 
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
