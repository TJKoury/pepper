C$Procedure T_TORUTL ( Torus utility package )
 
      SUBROUTINE T_TORUTL ( R,     CENTER, NORMAL, T, 
     .                      CURVE, DERIV,  TWIST     )
 
C$ Abstract
C
C     Umbrella routine for entry points that provide a central curve
C     for a torus.
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
C     DSK
C     PLATE
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      R
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      CURVE  ( 3 )
      DOUBLE PRECISION      DERIV  ( 3 )
      DOUBLE PRECISION      TWIST

C$ Brief_I/O
C
C     Variable  I/O  Entry points
C     --------  ---  --------------------------------------------------
C     R          I   T_TORSET
C     CENTER     I   T_TORSET
C     NORMAL     I   T_TORSET
C     T          I   T_TORCRV
C     CURVE      O   T_TORCRV
C     DERIV      O   T_TORCRV
C     TWIST      O   T_TORCRV
C
C$ Detailed_Input
C
C     See the entry points.           
C                         
C$ Detailed_Output
C
C     See the entry points.           
C                  
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called directly, the error
C         SPICE(BOGUSENTRY) is signaled.
C
C
C     See the entry points for descriptions of errors specific to those
C     routines.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This package contains two entry points:
C
C        T_TORSET {initialize central curve}
C        T_TORCRV {get curve and derivative}
C
C     These entry points are used by the test utility routine
C
C        T_TORUS
C
C     which is an entry point of
C
C        T_WRDSK2
C
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
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 26-APR-2016 (NJB)
C
C-&




C
C     SPICELIB functions
C
      DOUBLE PRECISION      TWOPI

      LOGICAL               RETURN

C
C     Local variables
C
      DOUBLE PRECISION      C
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SAVCTR ( 3 )
      DOUBLE PRECISION      SAVR 
      DOUBLE PRECISION      SAVX   ( 3 )
      DOUBLE PRECISION      SAVY   ( 3 )
      DOUBLE PRECISION      THETA

C
C     Saved variables
C
      SAVE                  SAVCTR
      SAVE                  SAVR
      SAVE                  SAVX
      SAVE                  SAVY

C
C     Initial values
C
      DATA                  SAVR / -1.D0 /


      CALL CHKIN  ( 'T_TORUTL'          )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_TORUTL'          )
      RETURN





C$Procedure T_TORCRV ( Torus curve )
 
      ENTRY T_TORCRV ( T, CURVE, DERIV, TWIST )
 
C$ Abstract
C
C     Return a point on a parameterized central curve of
C     a torus; return the curve's derivative at this point
C     as well. Return a twist value of zero.
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
C     DSK
C     PLATE
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      T
C     DOUBLE PRECISION      CURVE ( 3 )
C     DOUBLE PRECISION      DERIV ( 3 )
C     DOUBLE PRECISION      TWIST
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     T          I   The curve parameter. Values are in [0, 1].
C     CURVE      O   The value of the curve at T.
C     DERIV      O   The derivative of the curve with respect to T at T.
C     TWIST      O   The twist value at T; identically zero.
C
C$ Detailed_Input
C
C     T          is the value of the curve parameter. T is in the 
C                closed interval [0, 1].
C                         
C$ Detailed_Output
C
C     CURVE      is the value of the curve at T.
C
C     DERIV      is the value of the derivative of the curve with 
C                respect to T, at T.
C
C     TWIST      is the twist at T; this value is always set to zero.
C                See Particulars.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If T is outside the interval [0, 1 ], the error
C         SPICE(VALUEOUTOFRANGE) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine defines a central curve a torus. 
C
C     The argument list of this routine is compatible with the
C     requirements of the test utility routine
C
C        ZZPSTUBE
C
C     The output TWIST is provided because ZZPSTUBE requires it. (Twist
C     is used by ZZPSTUBE to orient cross sections of the tube it
C     creates.)
C
C
C     In a Cartesian coordinate system having the Z axis parallel to 
C     NORMAL, and having an origin coincident with CENTER, the torus
C     is the set of points
C
C                                2   2  1/2  2     2           2
C        { (x,y,z) : (  R  -  ( x + y  )    )  +  z   =  RCROSS  }
C     
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
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 26-APR-2016 (NJB)
C
C-&

C
C     Use discovery check-in.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF


      IF (  ( T .LT. 0.D0 ) .OR. ( T .GT. 1.D0 ) ) THEN

         CALL CHKIN ( 'T_TORCRV'                          )
         CALL SETMSG ( 'Parameter T was # but must be in '
     .   //            'the range [0, 1].'                )
         CALL ERRDP  ( '#',  T                            )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'           )
         CALL CHKOUT ( 'T_TORCRV'                         )
         RETURN

      END IF
 
      THETA = T * TWOPI() 

      C     = SAVR * COS(THETA)
      S     = SAVR * SIN(THETA)

      CALL VLCOM3 (  C, SAVX, S, SAVY, 1.D0, SAVCTR, CURVE )
      CALL VLCOM  ( -S, SAVX, C, SAVY,               DERIV )

      TWIST = 0.D0

      RETURN







C$Procedure T_TORSET ( Torus set-up )
 
      ENTRY T_TORSET ( R, CENTER, NORMAL )
 
C$ Abstract
C
C     Perform set-up functions for the entry point T_TORCRV.
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
C     DSK
C     PLATE
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      R
C     DOUBLE PRECISION      CENTER ( 3 )
C     DOUBLE PRECISION      NORMAL ( 3 )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     R          I   Radius of central curve.
C     CENTER     I   Center of a torus.
C     NORMAL     I   Normal vector to plane of central curve.
C
C$ Detailed_Input
C
C     R          is the radius of the central curve of a torus.
C                Each cross section is centered on this curve and
C                is normal to the derivative of the curve at the point
C                of intersection.
C
C     CENTER     is the center of the torus. The is the center of the
C                central curve of the torus. The line normal to the 
C                plane of the central curve and passing through CENTER
C                is the axis of azimuthal symmetry of the torus.
C
C     NORMAL     is a vector normal to the plane containing the 
C                central curve of the torus.
C
C
C$ Detailed_Output
C
C     None. This routine operates by side effects.
C                  
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If an orthonormal basis having its +Z axis parallel to 
C         NORMAL cannot be created, an error will be signaled by
C         a routine in the call tree of this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine saves the values of the input arguments R and CENTER
C     so that they may be used by the entry point T_TORCRV. It
C     generates an orthonormal basis having its +Z axis parallel to
C     NORMAL.
C
C     In a Cartesian coordinate system having the Z axis parallel to 
C     NORMAL, and having an origin coincident with CENTER, the torus
C     is the set of points
C
C                                2   2  1/2  2     2           2
C        { (x,y,z) : (  R  -  ( x + y  )    )  +  z   =  RCROSS  }
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
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 26-APR-2016 (NJB)
C
C-&

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_TORSET' )


      SAVR = R
      CALL VEQU ( CENTER, SAVCTR )

C
C     Generate basis vectors for the plane containing
C     the central curve of the torus.
C
      CALL FRAME ( NORMAL, SAVX, SAVY )

      CALL CHKOUT ( 'T_TORSET' )
      RETURN 
      END
