C$Procedure F_ZZPDPLTC ( ZZPDPLTC tests )
 
      SUBROUTINE F_ZZPDPLTC ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB geometry routine ZZPDPLTC.
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
C     This routine tests the private SPICELIB 
C     geometry routine ZZPDPLTC.
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
C-    TSPICE Version 1.0.0, 18-FEB-2017 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
     
      LOGICAL               ZZPDPLTC

C
C     Local Parameters
C

C
C     This value must match that in ZZPDPLTC.
C
      DOUBLE PRECISION      LIMIT
      PARAMETER           ( LIMIT = 1.D-2 )

C
C     Local Variables
C
C
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      P     ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      S
      DOUBLE PRECISION      XXPT
      DOUBLE PRECISION      XYPT

      LOGICAL               ISON
      LOGICAL               XISON

C
C     Saved values
C      
C     Initial values
C
 

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZPDPLTC' )




C***********************************************************************
C
C     Error cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RE is zero.' )


      RE    = 0.D0
      F     = 0.D0
      LAT   = 0.D0

      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'RE is negative.' )


      RE    = -1.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4
      
      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'F > 1.' )


      RE    = 3.D0
      RP    = 2.D0
      F     = 2.D0
      LAT   = PI()/4
      
      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )






C***********************************************************************
C
C     Oblate tests
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = PI/4; P(3) > 0' )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4
      
      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = -PI/4; P(3) > 0' )

 
      LAT   = -PI()/4
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .FALSE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) > 0' )

 
      LAT   = 0.D0
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = PI/4; P(3) < 0' )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  = -1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .FALSE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = -PI/4; P(3) < 0' )

 
      LAT   = -PI()/4
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) < 0' )

 
      LAT   = 0.D0
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )




C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = PI/4; P(3) = 0' )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  =  0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = -PI/4; P(3) = 0' )

 
      LAT   = -PI()/4
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) = 0' )

 
      LAT   = 0.D0
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )




C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) > 0, '
     .//           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) > 0, '
     .//           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The point is accepted on the basis of its Z component
C     having the correct sign.
C     
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) < 0, '
     .//           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = -1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) < 0, '
     .//           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = -1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .FALSE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) = 0, '
     .//           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) = 0, '
     .//           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The point is accepted because its Z component is zero.
C
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) = 0, '
     .//           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) = 0, '
     .//           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The point is accepted because its Z component is zero.
C     
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )



C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) = 0, '
     .//           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = 0.D0; P(3) = 0, '
     .//           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )





C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) > 0, '
     .//           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) > 0, '
     .//           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .FALSE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) < 0, '
     .//           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = -1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) < 0, '
     .//           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = -1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The point is accepted on the basis of the sign of its Z
C     component.
C     
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )



C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) > 0, '
     .//           'radius > XXPT.'                  )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) > 0, '
     .//           'radius < XXPT.'                   )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) < 0, '
     .//           'radius > XXPT.'                   )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = -1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) < 0, '
     .//           'radius < XXPT.'                   )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = -1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) = 0, '
     .//           'radius > XXPT.'                   )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) = 0, '
     .//           'radius < XXPT.'                   )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )





C***********************************************************************
C
C     Prolate tests
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Prolate case; LAT = PI/4; P(3) > 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4
      
      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Prolate case; LAT = -PI/4; P(3) > 0' )
 
      LAT   = -PI()/4
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note: the prolate case ALWAYS returns .TRUE.
C
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Prolate case; LAT = 0; P(3) > 0' )
 
      LAT   = 0.D0
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )
  
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )



C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Prolate case; LAT = PI/4; P(3) < 0' )

 
      LAT   = PI()/4
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  = -1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Prolate case; LAT = -PI/4; P(3) < 0' )

 
      LAT   = -PI()/4
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note: the prolate case ALWAYS returns .TRUE.
C
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Prolate case; LAT = 0; P(3) < 0' )
 
      LAT   = 0.D0
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )
  
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Prolate case; LAT = PI/4; P(3) = 0' )

 
      LAT   = PI()/4
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  =  0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Prolate case; LAT = -PI/4; P(3) = 0' )

 
      LAT   = -PI()/4
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note: the prolate case ALWAYS returns .TRUE.
C
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Prolate case; LAT = 0; P(3) = 0' )

  
      LAT   = 0.D0
       

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )
  
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )




C***********************************************************************
C
C     Sphere tests
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Sphere case; LAT = PI/4; P(3) > 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4
      
      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Sphere case; LAT = -PI/4; P(3) > 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -PI()/4
      
      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note: the prolate case ALWAYS returns .TRUE.
C
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Sphere case; LAT = 0; P(3) > 0' )

 
      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      P(1)  = 1.D0 
      P(2)  = 1.D0 
      P(3)  = 1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )
  
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )



C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Sphere case; LAT = PI/4; P(3) < 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  = -1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Sphere case; LAT = -PI/4; P(3) < 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -PI()/4
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  = -1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note: the prolate case ALWAYS returns .TRUE.
C
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Sphere case; LAT = 0; P(3) < 0' )

 
      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  = -1.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )
  
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Sphere case; LAT = PI/4; P(3) = 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  =  0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Sphere case; LAT = -PI/4; P(3) = 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -PI()/4
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  =  0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note: the prolate case ALWAYS returns .TRUE.
C
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


C
C --- Case: ------------------------------------------------------
C

      CALL TCASE ( 'Sphere case; LAT = 0; P(3) = 0' )

 
      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0
      
      P(1)  =  1.D0 
      P(2)  =  1.D0 
      P(3)  =  0.D0 

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )
  
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )



C
C --- Case: ------------------------------------------------------
C
 

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


