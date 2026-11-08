C$Procedure F_ZZLATBOX ( ZZLATBOX tests )
 
      SUBROUTINE F_ZZLATBOX ( OK )
 
C$ Abstract
C
C     Exercise the latitudinal volume element bounding
C     box routine ZZLATBOX.
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
C     This routine tests the DSK support routine ZZLATBOX. 
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
C-    TSPICE Version 1.0.0, 03-FEB-2015 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

C
C     Local Parameters 
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )


C
C     Local Variables 
C   
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XCTR   ( 3 )
      DOUBLE PRECISION      XLR
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XLZ
      DOUBLE PRECISION      XRAD

C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZLATBOX' )


C**********************************************************************
C
C     Error cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Bad longitude range: max is less than min ' 
     .//           'by more than 2*pi.'                        )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = -TWOPI() - 1.D-15

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()
      
      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      
      CALL CHCKXC ( .TRUE., 'SPICE(BADLONGITUDERANGE)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Latitude bounds out of order' )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  HALFPI()
      BOUNDS(2,2) = -HALFPI()
      
      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      
      CALL CHCKXC ( .TRUE., 'SPICE(BADLATITUDEBOUNDS)', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Minimum latitude is too small' )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) = -HALFPI() - 1.D-10
      BOUNDS(2,2) =  HALFPI()
      
      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      
      CALL CHCKXC ( .TRUE., 'SPICE(BADLATITUDERANGE)', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Maximum latitude is too large' )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI() + 1.D-10
      
      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      
      CALL CHCKXC ( .TRUE., 'SPICE(BADLATITUDERANGE)', OK )




C**********************************************************************
C
C     Normal cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Trivial case: element is sphere.' )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) = -HALFPI()
      BOUNDS(2,2) =  HALFPI()
      
      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XRAD = BOUNDS(2,3) * SQRT(3.D0)

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )

      XLR = 2 * BOUNDS(2,3)
      XLT = XLR
      XLZ = XLR

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )




C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is north polar cap with zero thickness.' )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  HALFPI()/2
      BOUNDS(2,2) =  HALFPI()
      
      BOUNDS(1,3) = 20.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )
      XCTR(3) = BOUNDS(2,3) * (( 1.D0 + SQRT(2.D0)/2 ) / 2 )


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = ( BOUNDS(2,3) * SQRT(2.D0)/2 ) * 2
      XLT = XLR
      XLZ = BOUNDS(2,3) * ( 1.D0 - SQRT(2.D0)/2 ) 

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is north polar cap with non-zero '
     .//           'thickness.' )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  HALFPI()/2
      BOUNDS(2,2) =  HALFPI()
      
      BOUNDS(1,3) = 19.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )
      XCTR(3) = (  BOUNDS(2,3) + ( BOUNDS(1,3)*SQRT(2.D0)/2 )  ) / 2


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = ( BOUNDS(2,3) * SQRT(2.D0)/2 ) * 2
      XLT = XLR
      XLZ = BOUNDS(2,3) - ( BOUNDS(1,3)*SQRT(2.D0)/2 ) 

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is south polar cap with zero thickness.' )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  -HALFPI()
      BOUNDS(2,2) =  -HALFPI()/2
      
      BOUNDS(1,3) = 20.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )
      XCTR(3) = -BOUNDS(2,3) * (( 1.D0 + SQRT(2.D0)/2 ) / 2 )


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = ( BOUNDS(2,3) * SQRT(2.D0)/2 ) * 2
      XLT = XLR
      XLZ = BOUNDS(2,3) * ( 1.D0 - SQRT(2.D0)/2 ) 

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is south polar cap with non-zero '
     .//           'thickness.' )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = TWOPI()

      BOUNDS(1,2) =  -HALFPI()
      BOUNDS(2,2) =  -HALFPI()/2
      
      BOUNDS(1,3) = 19.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, XCTR )
      XCTR(3) = -(  BOUNDS(2,3) + ( BOUNDS(1,3)*SQRT(2.D0)/2 )  ) / 2


      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )

      XLR = ( BOUNDS(2,3) * SQRT(2.D0)/2 ) * 2
      XLT = XLR
      XLZ = BOUNDS(2,3) - ( BOUNDS(1,3)*SQRT(2.D0)/2 ) 

      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is chunk above the X-Y plane, ' 
     .//           'lying over the +X axis.'               )


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) =  PI()/3

      BOUNDS(1,2) =  PI()/4
      BOUNDS(2,2) =  PI()/3
      
      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C      
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is chunk below the X-Y plane, ' 
     .//           'lying under the +X axis.'               )


      BOUNDS(1,1) = -PI()/3
      BOUNDS(2,1) =  PI()/3

      BOUNDS(1,2) = -PI()/3
      BOUNDS(2,2) = -PI()/4
      
      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C      
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is chunk above the X-Y plane, ' 
     .//           'lying over the -X axis. Longitude extent '
     .//           'is > pi.'                                 )


      BOUNDS(1,1) =  PI()/3
      BOUNDS(2,1) = -PI()/3

      BOUNDS(1,2) =  PI()/4
      BOUNDS(2,2) =  PI()/3
      
      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C      
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is chunk below the X-Y plane, ' 
     .//           'lying under the -X axis. Longitude extent '
     .//           'is > pi.'                                 )


      BOUNDS(1,1) =  PI()/3
      BOUNDS(2,1) = -PI()/3

      BOUNDS(1,2) =  -PI()/3
      BOUNDS(2,2) =  -PI()/4
      
      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C      
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is an equatorial belt that is '  
     .//           'symmetric about the X-Y plane.'        )


      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) =  -PI()/3
      BOUNDS(2,2) =   PI()/3
      
      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C      
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is an equatorial belt that is '  
     .//           'asymmetric about the X-Y plane.'        )


      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) =  -PI()/4
      BOUNDS(2,2) =   PI()/3
      
      BOUNDS(1,3) = 10.D0
      BOUNDS(2,3) = 20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C      
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is a slice ranging from the north '      
     .//           'to the south pole. Longitude extent is < pi.' )

      BOUNDS(1,1) =   3*PI()/4
      BOUNDS(2,1) =  -3*PI()/4

      BOUNDS(1,2) =  -PI()/2
      BOUNDS(2,2) =   PI()/2
      
      BOUNDS(1,3) =    1.D0
      BOUNDS(2,3) =   20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C      
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Element is a slice ranging from the north '      
     .//           'to the south pole. Longitude extent is > pi.' )

      BOUNDS(1,1) =    PI()/4
      BOUNDS(2,1) =  - PI()/4

      BOUNDS(1,2) =  -PI()/2
      BOUNDS(2,2) =   PI()/2
      
      BOUNDS(1,3) =    1.D0
      BOUNDS(2,3) =   20.D0

      CALL ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )      
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Generate expected outputs.
C      
      CALL T_TSTBOX ( BOUNDS, XCTR, XLR, XLT, XLZ, XRAD )

      TOL = TIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~', XCTR, 3, TOL, OK )


      CALL CHCKSD ( 'LT',     LT,     '~', XLT,  TOL, OK )
      CALL CHCKSD ( 'LR',     LR,     '~', XLR,  TOL, OK )
      CALL CHCKSD ( 'LZ',     LZ,     '~', XLZ,  TOL, OK )

      XRAD = SQRT( (XLR/2)**2 + (XLT/2)**2 + (XLZ/2)**2 )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XRAD, TOL, OK )



C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


C
C     Utility routine T_TSTBOX: create latitude bounding
C     box for testing. The bounds of segment may span the
C     X-Y plane.
C     
      SUBROUTINE T_TSTBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      IMPLICIT NONE

      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS

C
C     SPICELIB Functions
C
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN
 
C
C     Local parameters
C     
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'T_TSTBOX' )
 
C
C     Local variables
C
      DOUBLE PRECISION      CTR    ( 3, 2 )
      DOUBLE PRECISION      DIAG   ( 3 )
      DOUBLE PRECISION      EXTENT ( 3, 2 )
      DOUBLE PRECISION      H
      DOUBLE PRECISION      LOCBDS ( 2, 3 )
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXRC
      DOUBLE PRECISION      MAXZ
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINRC
      DOUBLE PRECISION      MINZ
      DOUBLE PRECISION      R
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2

      INTEGER               I



      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )

      MINLAT = BOUNDS(1,2)
      MAXLAT = BOUNDS(2,2)
      
      IF (      ( MINLAT .GE. 0.D0 )
     .     .OR. ( MAXLAT .LT. 0.D0 ) ) THEN
C
C        The volume element doesn't cross the X-Y plane.
C        We can delegate the job.
C
         CALL T_MKBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )

         CALL CHKOUT ( RNAME )
         RETURN

      END IF

C
C     The volume element crosses the X-Y plane. Divide the
C     element into upper and lower parts; find bounding boxes
C     for each.
C
      DO I = 1, 2

         CALL MOVED ( BOUNDS, 6, LOCBDS )

         LOCBDS(I,2) = 0.D0
      
         CALL T_MKBOX ( LOCBDS,      CTR(1,I),    EXTENT(1,I), 
     .                  EXTENT(2,I), EXTENT(3,I), R           )

      END DO

C
C     At this point the longitude of the center and the tangential
C     extent of the box are known. We'll need to recompute the 
C     radius and Z component of the center and the radial and Z
C     extents of the box. 
C
      LT  =  EXTENT(2,1)

C
C     Start with the Z values. Recall that the two boxes
C     are bounded by the X-Y plane, which simplifies our
C     calculations. The box at index 1 is the upper box.
C
      MAXZ      =  EXTENT(3,1) 
      MINZ      = -EXTENT(3,2)
      LZ        =  MAX ( 0.D0,  MAXZ - MINZ )
      CENTER(3) =  MAXZ - (LZ/2)

C
C     Both boxes have the maximum radial component, so we can
C     compute this component using the first box alone. The
C     minimum radial component must be derived using both boxes.
C 
C     Note the radial component of the box center is always
C     positive, and the longitude of the box center is always
C     the same as that of the midpoint of the centers of any
C     arc of constant latitude on the outer bounding sphere.
C      
      CALL RECCYL ( CTR(1,1), R1, LON, H )

      MAXRC     = R1  +  EXTENT(1,1)/2 

      CALL RECCYL ( CTR(1,2), R2, LON, H )


      MINRC     = MIN(  R1 - EXTENT(1,1)/2,
     .                  R2 - EXTENT(1,2)/2  )
    
      LR        = MAX( 0.D0,  MAXRC - MINRC )
      
      H         = CENTER(3)

      CALL CYLREC ( MAXRC - LR/2,  LON,  H,  CENTER )

      CALL VPACK ( LT/2, LR/2, LZ/2, DIAG )

      RADIUS = VNORM( DIAG )

      CALL CHKOUT ( RNAME )
      RETURN
      END



C
C     Utility routine T_MKBOX: create box using coordinates
C     of the corners of a volume element. This technique
C     is valid only for "small" elements, for which the
C     bounding box is not tangent at interior points of
C     the elements' surface. Note that this routine cannot
C     be used for elements that cross the X-Y plane.
C     
      SUBROUTINE T_MKBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )
      IMPLICIT NONE

      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS

C
C     SPICELIB Functions
C
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      DPMIN
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN
 
C
C     Local parameters
C     
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME  = 'T_MKBOX' )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

C
C     Local variables
C
      DOUBLE PRECISION      CORNER ( 3, 2, 2, 2 )
      DOUBLE PRECISION      CPLANE ( UBPL )
      DOUBLE PRECISION      DIAG   ( 3 )
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MAXRC
      DOUBLE PRECISION      MAXZ
      DOUBLE PRECISION      MIDARC ( 3 )
      DOUBLE PRECISION      MIDLON
      DOUBLE PRECISION      MIDPT  ( 3 )
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MINRC
      DOUBLE PRECISION      MINZ
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RADVEC ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               K
 
      SAVE                  Z

      DATA                  Z / 0.D0, 0.D0, 1.D0 /
      
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )

      MINLAT = BOUNDS(1,2)

      MINLON = BOUNDS(1,1)
      MAXLON = BOUNDS(2,1)

      IF ( MAXLON .LE. MINLON ) THEN
         MAXLON = MAXLON + TWOPI()
      END IF

      DLON   = MAXLON - MINLON
      MIDLON = MINLON + DLON/2

C
C     Compute Cartesian coordinates of the box corners.
C
      DO I = 1, 2

         LON = BOUNDS(I,1)

         DO J = 1, 2

            LAT = BOUNDS(J,2)

            DO K = 1, 2
               
               R = BOUNDS(K,3)
               
               CALL LATREC ( R, LON, LAT, CORNER(1,I,J,K) )

            END DO

         END DO

      END DO

C     
C     Pick the latitude of the segment so that its length
C     is the transverse length of the box.
C
      IF ( MINLAT .GE. 0.D0 ) THEN
C
C        Use the lower pair of corners on the sphere of maximum
C        radius.
C         
         I = 1
      ELSE
         I = 2
      END IF

C
C     Find the midpoint of the latitude boundary of maximum
C     radius that is closest to the X-Y plane. This is the
C     point with the greatest component in the radial direction.
C
      CALL LATREC ( BOUNDS(2,3), MIDLON, BOUNDS(I,2), MIDARC )

C
C     Create a central plane for the box: this plane contains
C     the midpoint of a line segment connecting a pair of
C     corners at the same latitude and radius, and is normal
C     to the segment.

      CALL VSUB ( CORNER(1,2,I,2), CORNER(1,1,I,2), NORMAL )
      
      CALL VLCOM ( 0.5D0, CORNER(1,2,1,2), 
     .             0.5D0, CORNER(1,1,1,2), MIDPT )

      CALL NVP2PL ( NORMAL, MIDPT, CPLANE )


      IF ( DLON .LE. PI() ) THEN
C
C        The segment connecting the two corners selected above
C        has length equal to the tangential extent of the element.
C
         LT = VNORM( NORMAL )

      ELSE
C
C        The widest part of the element is at the longitudes 90 degrees
C        away from MIDLON. The width is just the width of the outer
C        circle formed by projecting the element orthogonally onto the
C        X-Y plane.
C
         LT = 2.D0 * BOUNDS(2,3) * COS( BOUNDS(I,2) ) 

      END IF

C
C     For each corner on the volume face of minimum longitude, 
C     compute the minimum and maximum Z values of the corners.
C
      MINZ = DPMAX()
      MAXZ = DPMIN()

      DO J = 1, 2

         DO K = 1, 2

            MINZ = MIN( MINZ, CORNER(3,1,J,K) )
            MAXZ = MAX( MAXZ, CORNER(3,1,J,K) )

         END DO

      END DO

C
C     Compute the Z extent of the element.
C
      LZ = MAX( MAXZ - MINZ,  0.D0 )

C
C     Let RADVEC be a unit vector in the radial direction.
C      
      CALL UCRSS ( NORMAL, Z, RADVEC )

C
C     Find the component of MIDARC in the RADVEC (radial) direction.
C
      MAXRC = VDOT ( MIDARC, RADVEC )


      IF ( DLON .LT. PI() ) THEN
C
C        For the corners on the intersection of the volume face of
C        minimum longitude and sphere of minimum radius, compute the
C        corners' components in the radial direction. Select the
C        minimum component.
C
         MINRC = MIN(  VDOT( CORNER(1,1,1,1), RADVEC ),
     .                 VDOT( CORNER(1,1,2,1), RADVEC )  )

      ELSE
C
C        The element "wraps" around the Z axis; the points having
C        minimum tangential components lie on the outer sphere.
C
         MINRC = MIN(  VDOT( CORNER(1,1,1,2), RADVEC ),
     .                 VDOT( CORNER(1,1,2,2), RADVEC )  )

      END IF

C
C     We now have the extent of the element in the radial direction.
C
      LR = MAX( MAXRC - MINRC,  0.D0 )

C
C     MIDARC lies on the central, vertical plane of the box, at the end
C     of maximum tangential component. We can derive the coordinates of
C     the center of the box from MIDARC, MINZ, and the box extents.
C
      CALL CYLREC( MAXRC - (LR/2),   MIDLON,   MINZ + (LZ/2), CENTER )


      CALL VPACK ( LT/2, LR/2, LZ/2, DIAG )

      RADIUS = VNORM( DIAG )

      CALL CHKOUT ( RNAME )
      RETURN
      END


