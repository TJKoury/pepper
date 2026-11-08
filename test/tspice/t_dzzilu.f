C$Procedure T_DZZILU ( Illumination angle rates )

      SUBROUTINE T_DZZILU ( METHOD, TARGET, ILLUM,  ET,  
     .                      FIXREF, ABCORR, OBSRVR, SPOINT,
     .                      NORMAL, DPHASE, DINCDN, DEMSSN  )
  
C$ Abstract
C
C     Compute by numeric methods the derivatives with respect to 
C     time of illumination angles.
C
C     This is a TSPICE utility that supports F_ZZILUSTA.
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
C     PCK
C     TIME
C     SPK
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLUM
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      DPHASE
      DOUBLE PRECISION      DINCDN
      DOUBLE PRECISION      DEMSSN

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     METHOD     I   Computation method.
C     TARGET     I   Name of target body associated with surface point.
C     ILLUM      I   Name of illumination source.
C     ET         I   Observation epoch (TDB).
C     FIXREF     I   Body-fixed reference frame.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Name of observer.
C     SPOINT     I   Surface point.
C     NORMAL     I   Outward normal vector at surface point.
C     DPHASE     O   Phase angle rate.
C     DINCDN     O   Illumination source incidence angle rate.
C     DEMSSN     O   Emission angle rate.
C
C$ Detailed_Input
C                                 
C     METHOD         is a string specifying the computation method to
C                    be used by this routine. The only value currently
C                    allowed is 'ELLIPSOID'. This indicates that the
C                    target body shape is modeled as an ellipsoid.
C
C                    Case and leading and trailing blanks are not
C                    significant in METHOD.
C
C     TARGET         is the name of the target body associated with the
C                    surface point SPOINT. TARGET may be a body name or
C                    an ID code provided as as string.
C
C     ILLUM          is the name of the illumination source used to
C                    define the illumination angles computed by this
C                    routine. ILLUM may be a body name or an ID code
C                    provided as as string.
C
C     ET             is the observation time. ET is expressed as
C                    seconds past J2000 TDB.
C
C     FIXREF         is the name of the body-centered, body-fixed
C                    reference frame relative to which SPOINT is
C                    specified. The frame's center must coincide with
C                    TARGET.
C
C     ABCORR         indicates the aberration corrections to be
C                    applied. Only reception corrections are supported.
C                    See the header of ILUMIN for a discussion of
C                    aberration corrections used in illumination angle
C                    computations.
C
C     OBSRVR         is the name of the observing body. OBSRVR may be a
C                    body name or an ID code provided as as string.
C
C     SPOINT         is a 3-vector containing the cartesian coordinates
C                    of the surface point at which the illumination
C                    angle rates are to be computed. SPOINT is
C                    expressed in the body-centered, body-fixed frame
C                    designated by FIXREF (see description above).
C
C                    Units are km.
C
C     NORMAL         is an outward normal vector to be used for
C                    emission angle and illumination source incidence
C                    angle calculations. NORMAL should be orthogonal
C                    at SPOINT to the target body's surface.
C                       
C$ Detailed_Output
C
C     DPHASE         is the rate of change of the phase angle with
C                    respect to TDB, evaluated at ET.
C
C     DINCDN         is the rate of change of the illumination source
C                    incidence angle with respect to TDB, evaluated at
C                    ET.
C
C     DEMSSN         is the rate of change of the emission angle with
C                    respect to TDB, evaluated at ET.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the computation method is not recognized, the error
C         will be signaled by a routine in the call tree of this
C         routine.
C
C     2)  If ABCORR specifies a transmission aberration correction, the
C         error will be signaled by a routine in the call tree of this
C         routine.
C
C     3)  If an error occurs while looking up a state vector, the
C         error will be signaled by a routine in the call tree of
C         this routine.
C
C$ Files
C
C     See GFILUM.
C
C$ Particulars
C
C     This is a specialized routine intended for use only
C     by F_ZZILUSTA.
C     
C$ Examples
C
C     See usage in F_ZZILUSTA.
C
C$ Restrictions
C
C     1) This routine is intended for use only by the GF subsystem.
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
C-    SPICELIB Version 2.1.0, 27-AUG-2013 (NJB)
C
C        Bug fix: T_ILUINI is now called using function
C        call syntax.
C
C-    SPICELIB Version 2.0.0, 09-APR-2012 (NJB)
C
C-&
 

C
C     External routines
C
      DOUBLE PRECISION      T_ILUINI
      EXTERNAL              T_ILUINI

      DOUBLE PRECISION      T_ILUPHS
      EXTERNAL              T_ILUPHS

      DOUBLE PRECISION      T_ILUINC
      EXTERNAL              T_ILUINC

      DOUBLE PRECISION      T_ILUEMI
      EXTERNAL              T_ILUEMI

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      DOUBLE PRECISION      DELTA
      PARAMETER           ( DELTA = 240.D0 )

C
C     Local variables
C     
      DOUBLE PRECISION      F
      DOUBLE PRECISION      RETVAL

      INTEGER               NTERMS
      INTEGER               NRTAIN


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'T_DZZILU' )
     
C
C     Initialize angle computation functions:
C     store inputs to ZZILUMIN.
C      
      RETVAL =  T_ILUINI ( METHOD, TARGET, ILLUM,  FIXREF,
     .                     ABCORR, OBSRVR, SPOINT, NORMAL )
      
      IF ( FAILED() ) THEN
         CALL CHKOUT( 'T_DZZILU' )
         RETURN
      END IF
 
C
C     Set the number of terms in the Chebyshev expansions
C     we'll use. Set the number of retained terms as well.
C
      NTERMS = 25
      NRTAIN = 6
C
C     Estimate the derivatives of the illumination angles
C     at ET.
C     
      CALL T_DCHEB ( T_ILUPHS, ET, DELTA, NTERMS, NRTAIN, F, DPHASE )
      CALL T_DCHEB ( T_ILUINC, ET, DELTA, NTERMS, NRTAIN, F, DINCDN )
      CALL T_DCHEB ( T_ILUEMI, ET, DELTA, NTERMS, NRTAIN, F, DEMSSN )

      CALL CHKOUT ( 'T_DZZILU' )
      RETURN
      END
