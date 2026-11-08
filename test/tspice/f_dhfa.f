C$Procedure F_DHFA ( DHFA, DVSEP family tests )

      SUBROUTINE F_DHFA ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routines
C     
C        ZZGFDHFA
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
C-    TSPICE Version 1.0.1 06-JUL-2009 (EDW)
C
C        Rename of the ZZDHA call to DHFA.
C
C-    TSPICE Version 1.0.0, 15-OCT-2008 (EDW)
C
C-&

C
C     SPICELIB functions
C

      DOUBLE PRECISION      DHFA
      DOUBLE PRECISION      T_RANDD
      DOUBLE PRECISION      VNORM

C     
C     Local parameters
C

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-12 )
      
      DOUBLE PRECISION      RBODY
      PARAMETER           ( RBODY = 1000.D0 )

      DOUBLE PRECISION      ZERO6 ( 6 )
      DOUBLE PRECISION      ONE6  ( 6 )
      DOUBLE PRECISION      VAL
      DOUBLE PRECISION      ADOT
      DOUBLE PRECISION      R
      DOUBLE PRECISION      BODYR
      DOUBLE PRECISION      STATE(6)

      INTEGER               SEED

      DATA                  ZERO6 /0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0/
      DATA                  ONE6  /1.D0, 1.D0, 1.D0, 1.D0, 1.D0, 1.D0/
      
      SAVE

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DHFA' )

C
C     Case 1
C
      CALL TCASE ( 'Non-positive body radius.' )

      SEED = -1804030
      BODYR = T_RANDD( -199999999.D0, 0.D0, SEED )

      VAL   = DHFA( ONE6, BODYR )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUS)', OK )



C
C     Case 2
C
      CALL TCASE ( 'Position vector equals zero vector.' )

      VAL   = DHFA( ZERO6, RBODY )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )



C
C     Case 3
C
      CALL TCASE ( 'Range less-than body radius.' )

      VAL   = DHFA( ONE6, RBODY )
      CALL CHCKXC ( .TRUE., 'SPICE(BADGEOMETRY)', OK )



C
C     Case 4
C
C     Circular orbits; constant half angle. Physically 
C     realistic orbits not required, only geometry with 
C     -   -
C     R . V = 0
C
      CALL TCASE ( 'DHFA Circular orbits.' )

C         .
C     x * x = 1
C         .
C     y * y = -1/2
C         .
C     z * z = -1/2
C
      STATE(1) = 1.D08
      STATE(2) = 1.1D12
      STATE(3) = -2.3D10
      STATE(4) = 1.D-08
      STATE(5) = -0.5D0 * 1.1D-12
      STATE(6) =  0.5D0 * 2.3D10

      VAL   = DHFA( STATE, RBODY )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'DHFA Circular', VAL, '~', 0.D0, TIGHT, OK )



C
C     Case 5
C
C     Linear orbits. 
C     Physically realistic orbits not required, only geometry with 
C     -   -
C     R . V = 1
C
      CALL TCASE ( 'DHFA Linear orbits.' )

C         .
C     x * x = 1/3
C         .
C     y * y = 1/3
C         .
C     z * z = 1/3
C
      STATE(1) = 1.D04
      STATE(2) = 1.1D08
      STATE(3) = -2.3D06
      STATE(4) =  (1.D0/3.D0) * 1.D0-4
      STATE(5) =  (1.D0/3.D0) * 1.1D0-8
      STATE(6) = -(1.D0/3.D0) * 2.3D0-6

C
C     Reduce the function for the half angle derivative given
C     -   -
C     R . V = 1
C
C     to the form
C
C     d              - body_radius                 1
C     --(ALPHA) =  --------------------- *      ------
C     dt                  2             2  1/2       2
C                   (range - body_radius )      range
C
      R    =  VNORM(STATE(1))
      ADOT = -RBODY/DSQRT( R**2 - RBODY**2)
      ADOT =  ADOT/R**2

      VAL  = DHFA( STATE, RBODY )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'DHFA Linear', VAL, '~', ADOT, TIGHT, OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END

